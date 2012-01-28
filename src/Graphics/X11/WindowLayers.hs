module Graphics.X11.WindowLayers(
	Field,
	Layer,
	Character,

	openField,
	closeField,
	layerSize,

	addLayer,
	addCharacter,

	drawLine,
	drawCharacter,
	drawCharacterAndLine,

	undoLayer,
	clearLayer,

	forkIOX
) where

import Graphics.X11(
	Display, Window, Pixmap, Atom, GC, Point(..), Dimension,

	openDisplay, closeDisplay, flush, defaultScreen, rootWindow,
	whitePixel, blackPixel,	defaultDepth,
	createSimpleWindow, mapWindow, createPixmap, internAtom, createGC,

	setForeground, copyArea,
	fillRectangle, fillPolygon, nonconvex, coordModeOrigin,

	setWMProtocols, selectInput, allocaXEvent, nextEvent,
	keyPressMask, exposureMask,

	getGeometry, initThreads
 )
import qualified Graphics.X11 as X (drawLine)
import Graphics.X11.Xlib.Extras(Event(..), getEvent)

import Data.IORef(IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Bits((.|.))
import Data.Convertible(convert)
import Data.List.Tools(modifyAt)

import Control.Monad(replicateM, forM_)
import Control.Monad.Tools(doWhile_)
import Control.Arrow((***))
import Control.Concurrent(forkIO, ThreadId)

data Field = Field{
	fDisplay :: Display,
	fWindow :: Window,
	fGC :: GC,
	fGCBG :: GC,
	fDel :: Atom,
	fUndoBuf :: Pixmap,
	fBG :: Pixmap,
	fBuf :: Pixmap,
	fWidth :: IORef Dimension,
	fHeight :: IORef Dimension,
	fBuffed :: IORef [IO ()],
	fLayers :: IORef [[Bool -> IO ()]],
	fCharacters :: IORef [IO ()]
 }

data Layer = Layer{
	layerField :: Field,
	layerId :: Int
 }

data Character = Character{
	characterField :: Field,
	characterId :: Int
 }

forkIOX :: IO () -> IO ThreadId
forkIOX = (initThreads >>) . forkIO

openField :: IO Field
openField = do
	_ <- initThreads
	dpy <- openDisplay ""
	del <- internAtom dpy "WM_DELETE_WINDOW" True
	let	scr = defaultScreen dpy
	root <- rootWindow dpy scr
	(_, _, _, rWidth, rHeight, _, _) <- getGeometry dpy root
	let	black = blackPixel dpy scr
		white = whitePixel dpy scr
		depth = defaultDepth dpy scr
	bufs <- replicateM 3 $ createPixmap dpy root rWidth rHeight depth
	win <- createSimpleWindow dpy root 0 0 rWidth rHeight 1 black white
	[gc, gcBG] <- replicateM 2 $ createGC dpy win
	setForeground dpy gcBG 0xffffff
	forM_ bufs $ \bf -> fillRectangle dpy bf gcBG 0 0 rWidth rHeight
	setWMProtocols dpy win [del]
	selectInput dpy win $ exposureMask .|. keyPressMask
	mapWindow dpy win
	[widthRef, heightRef] <- mapM newIORef [rWidth, rHeight]
	buffActions <- newIORef []
	layerActions <- newIORef []
	characterActions <- newIORef []
	let f = Field{
		fDisplay = dpy,
		fWindow = win,
		fGC = gc,
		fGCBG = gcBG,
		fDel = del,
		fUndoBuf = head bufs,
		fBG = bufs !! 1,
		fBuf = bufs !! 2,
		fWidth = widthRef,
		fHeight = heightRef,
		fBuffed = buffActions,
		fLayers = layerActions,
		fCharacters = characterActions
	 }
	_ <- forkIOX $ runLoop f
	flushWin f
	return f

runLoop :: Field -> IO ()
runLoop f = (>> closeField f) $	doWhile_ $ allocaXEvent $ \e -> do
	nextEvent (fDisplay f) e
	ev <- getEvent e
	case ev of
		ExposeEvent{} -> do
			(_, _, _, width, height, _, _) <-
				getGeometry (fDisplay f) (fWindow f)
			writeIORef (fWidth f) width
			writeIORef (fHeight f) height
			redrawAll f
			return True
		KeyEvent{} -> return True
		ClientMessageEvent{} ->
			return $ convert (head $ ev_data ev) /= fDel f
		_ -> return True

closeField :: Field -> IO ()
closeField = closeDisplay . fDisplay

layerSize :: Layer -> IO (Double, Double)
layerSize = fieldSize . layerField

fieldSize :: Field -> IO (Double, Double)
fieldSize w = fmap (fromIntegral *** fromIntegral) $ fieldSizeRaw w

fieldSizeRaw :: Field -> IO (Dimension, Dimension)
fieldSizeRaw w = do
	width <- readIORef $ fWidth w
	height <- readIORef $ fHeight w
	return (width, height)

addLayer :: Field -> IO Layer
addLayer f = do
	ls <- readIORef $ fLayers f
	writeIORef (fLayers f) (ls ++ [[]])
	modifyIORef (fBuffed f) (++ [return ()])
	return Layer{layerField = f, layerId = length ls}

addCharacter :: Field -> IO Character
addCharacter f = do
	cs <- readIORef $ fCharacters f
	writeIORef (fCharacters f) (cs ++ [return ()])
	return Character{characterField = f, characterId = length cs}

drawLine :: Layer -> Double -> Double -> Double -> Double -> IO ()
drawLine l@Layer{layerField = f} x1_ y1_ x2_ y2_ = do
	(x1, y1) <- convertPos f x1_ y1_
	(x2, y2) <- convertPos f x2_ y2_
	lineWin f x1 y1 x2 y2
	addLayerAction l $ \buf -> do
		(x1', y1') <- convertPos f x1_ y1_
		(x2', y2') <- convertPos f x2_ y2_
		if buf	then lineUndoBuf f x1' y1' x2' y2'
			else lineWin f x1' y1' x2' y2'

undoN :: Int
undoN = 100

addLayerAction :: Layer -> (Bool -> IO ()) -> IO ()
addLayerAction Layer{layerField = f, layerId = lid} act = do
	ls <- readIORef $ fLayers f
	if length (ls !! lid) > undoN
		then do	head (ls !! lid) True
			buffed <- readIORef $ fBuffed f
			writeIORef (fBuffed f) $ 
				modifyAt buffed lid (>> head (ls !! lid) True)
			writeIORef (fLayers f) $
				modifyAt ls lid $ (++ [act]) . tail
		else writeIORef (fLayers f) $ modifyAt ls lid (++ [act])

convertPos :: Field -> Double -> Double -> IO (Double, Double)
convertPos f x y = do
	(width, height) <- fieldSize f
	return (x + width / 2, - y + height / 2)

clearLayer :: Layer -> IO ()
clearLayer l@Layer{layerField = f, layerId = lid} = do
	setExposeAction f l (const $ const $ return ())
	buffed <- readIORef $ fBuffed f
	writeIORef (fBuffed f) $
		take lid buffed ++ [return ()] ++ drop (lid + 1) buffed
	redrawAll f

redrawAll :: Field -> IO ()
redrawAll f = do
	redrawBuf f
	redraw f
	flushWin f

redrawBuf :: Field -> IO ()
redrawBuf f = do
	clearUndoBuf f
	readIORef (fBuffed f) >>= sequence_

setExposeAction :: Field -> Layer -> (Field -> Bool -> IO ()) -> IO ()
setExposeAction w@Field{fLayers = we} Layer{layerId = lid} act = do
	ls <- readIORef we
	writeIORef we $ take lid ls ++ [[act w]] ++ drop (lid + 1) ls

undoLayer :: Layer -> IO ()
undoLayer Layer{layerField = w, layerId = lid} = do
	ls <- readIORef $ fLayers w
	writeIORef (fLayers w) $ take lid ls ++ [init (ls !! lid)] ++ drop (lid + 1) ls
	redraw w

redraw :: Field -> IO ()
redraw w = do
	undoBufToBG w
	readIORef (fLayers w) >>= mapM_ ($ False) . concat
	readIORef (fCharacters w) >>= sequence_

setCharacter :: Field -> Character -> IO () -> IO ()
setCharacter w c act = do
	bgToBuf w
	setCharacterAction w c act
	readIORef (fCharacters w) >>= sequence_

setCharacterAction :: Field -> Character -> IO () -> IO ()
setCharacterAction Field{fCharacters = wc} Character{characterId = cid} act = do
	cs <- readIORef wc
	writeIORef wc $ take cid cs ++ [act] ++ drop (cid + 1) cs

undoBufToBG :: Field -> IO ()
undoBufToBG w = do
	(width, height) <- fieldSizeRaw w
	copyArea (fDisplay w) (fUndoBuf w) (fBG w) (fGC w) 0 0 width height 0 0

bgToBuf :: Field -> IO ()
bgToBuf w = do
	(width, height) <- fieldSizeRaw w
	copyArea (fDisplay w) (fBG w) (fBuf w) (fGC w) 0 0 width height 0 0

bufToWin :: Field -> IO ()
bufToWin w = do
	(width, height) <- fieldSizeRaw w
	copyArea (fDisplay w) (fBuf w) (fWindow w) (fGC w) 0 0 width height 0 0

fillPolygonBuf :: Field -> [(Double, Double)] -> IO ()
fillPolygonBuf w ps = do
	(width, height) <- fieldSize w
	let	dtp (x, y) = Point (round $ x + width / 2) (round $ - y + height / 2)
	fillPolygon (fDisplay w) (fBuf w) (fGC w) (map dtp ps) nonconvex coordModeOrigin

drawCharacter :: Character -> [(Double, Double)] -> IO ()
drawCharacter c@Character{characterField = w} ps = do
	setCharacter w c (fillPolygonBuf w ps)
	flushWin w

drawCharacterAndLine ::	Character -> [(Double, Double)] -> (Double, Double) ->
		(Double, Double) -> IO ()
drawCharacterAndLine c@Character{characterField = w} ps (x1, y1) (x2, y2) = do
	setCharacter w c (fillPolygonBuf w ps >> lineBuf w x1 y1 x2 y2)
	flushWin w

lineWin :: Field -> Double -> Double -> Double -> Double -> IO ()
lineWin w x1_ y1_ x2_ y2_ = do
	X.drawLine (fDisplay w) (fBG w) (fGC w) x1 y1 x2 y2
	bgToBuf w
	readIORef (fCharacters w) >>= sequence_
	where	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]

lineUndoBuf :: Field -> Double -> Double -> Double -> Double -> IO ()
lineUndoBuf w x1_ y1_ x2_ y2_ =
	X.drawLine (fDisplay w) (fUndoBuf w) (fGC w) x1 y1 x2 y2
	where	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]

lineBuf :: Field -> Double -> Double -> Double -> Double -> IO ()
lineBuf w x1__ y1__ x2__ y2__ = do
	(x1_, y1_) <- convertPos w x1__ y1__
	(x2_, y2_) <- convertPos w x2__ y2__
	let	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]
	X.drawLine (fDisplay w) (fBuf w) (fGC w) x1 y1 x2 y2

clearUndoBuf :: Field -> IO ()
clearUndoBuf w = fieldSizeRaw w >>=
	uncurry (fillRectangle (fDisplay w) (fUndoBuf w) (fGCBG w) 0 0)

flushWin :: Field -> IO ()
flushWin f = do
	bufToWin f
	flush $ fDisplay f

{-
changeColor :: Win -> Pixel -> IO ()
changeColor w = setForeground (fDisplay w) (fGC w)

clearBG :: Win -> IO ()
clearBG w = fieldSizeRaw w >>=
	uncurry (fillRectangle (fDisplay w) (fBG w) (fGCWhite w) 0 0)
-}
