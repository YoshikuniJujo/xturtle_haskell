module Graphics.X11.WindowLayers (
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

import Control.Monad.Tools(doWhile_)
import Control.Monad(replicateM, forM_)
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
	let w = Field{
		fDisplay = dpy,
		fWindow = win,
		fGC = gc,
		fGCBG = gcBG,
		fDel = del,
		fUndoBuf = bufs !! 0,
		fBG = bufs !! 1,
		fBuf = bufs !! 2,
		fWidth = widthRef,
		fHeight = heightRef,
		fBuffed = buffActions,
		fLayers = layerActions,
		fCharacters = characterActions
	 }
	_ <- forkIO $ runField w
	flushWin w
	return w

runField :: Field -> IO ()
runField w =
 (>> closeDisplay (fDisplay w)) $ (initThreads >>) $
	    withEvent w $ \ev ->
		case ev of
			ExposeEvent{} -> do
				(_, _, _, width, height, _, _) <-
					getGeometry (fDisplay w) (fWindow w)
				writeIORef (fWidth w) width
				writeIORef (fHeight w) height
				clearUndoBuf w
				readIORef (fBuffed w) >>= sequence_
				undoBufToBG w
				readIORef (fLayers w) >>= mapM_ ($ False) . concat
				readIORef (fCharacters w) >>= sequence_
				bufToWin w
				flushWin w
				return True
			KeyEvent{} -> return True
			ClientMessageEvent{} ->
				return $ not $ isDeleteEvent w ev
			_ -> return True
	where
	withEvent w' act = doWhile_ $ allocaXEvent $ \e -> do
		nextEvent (fDisplay w') e
		getEvent e >>= act
	isDeleteEvent w' ev@ClientMessageEvent{} =
		convert (head $ ev_data ev) == fDel w'
	isDeleteEvent _ _ = False

closeField :: Field -> IO ()
closeField = closeDisplay . fDisplay

undoN :: Int
undoN = 100

clearLayer :: Layer -> IO ()
clearLayer l@Layer{layerField = w, layerId = lid} = do
	setExposeAction w l (const $ const $ return ())
	buffed <- readIORef $ fBuffed w
	writeIORef (fBuffed w) $
		take lid buffed ++ [return ()] ++ drop (lid + 1) buffed
	nBuffed <- readIORef $ fBuffed w
	clearUndoBuf w
	sequence_ nBuffed
	redraw w
	bufToWin w
	flushWin w

addExposeAction :: Field -> Layer -> (Field -> Bool -> IO ()) -> IO ()
addExposeAction w@Field{fLayers = we} Layer{layerId = lid} act = do
	ls <- readIORef we
	let	theLayer = ls !! lid
		newLayer = theLayer ++ [act w]
	if length newLayer > undoN
		then do	head newLayer True
			buffed <- readIORef $ fBuffed w
			writeIORef (fBuffed w) $ take lid buffed ++
				[buffed !! lid >> head newLayer True] ++
				drop (lid + 1) buffed
			writeIORef we $ take lid ls ++ [tail newLayer] ++ drop (lid + 1) ls
		else writeIORef we $ take lid ls ++ [newLayer] ++ drop (lid + 1) ls

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
	bgToBuf w
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

addLayer :: Field -> IO Layer
addLayer f@Field{fLayers = we, fBuffed = wb} = do
	ls <- readIORef we
	modifyIORef we (++ [[]])
	modifyIORef wb (++ [return ()])
	return Layer{layerField = f, layerId = length ls}

addCharacter :: Field -> IO Character
addCharacter f@Field{fCharacters = wc} = do
	cs <- readIORef wc
	modifyIORef wc (++ [return ()])
	return Character{characterId = length cs, characterField = f}

layerSize :: Layer -> IO (Double, Double)
layerSize = fieldSize . layerField

fieldSize :: Field -> IO (Double, Double)
fieldSize w = fmap (fromIntegral *** fromIntegral) $ fieldSizeRaw w

fieldSizeRaw :: Field -> IO (Dimension, Dimension)
fieldSizeRaw w = do
	width <- readIORef $ fWidth w
	height <- readIORef $ fHeight w
	return (width, height)

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
	bufToWin w
	flushWin w

drawCharacterAndLine ::	Character -> [(Double, Double)] -> (Double, Double) ->
		(Double, Double) -> IO ()
drawCharacterAndLine c@Character{characterField = w} ps (x1, y1) (x2, y2) = do
	setCharacter w c (fillPolygonBuf w ps >> lineBuf w x1 y1 x2 y2)
	bufToWin w
	flushWin w

drawLine :: Layer -> Double -> Double -> Double -> Double -> IO ()
drawLine l@Layer{layerField = w} x1_ y1_ x2_ y2_ = do
	(width, height) <- fieldSize w
	let	x1 = x1_ + (width / 2)
		x2 = x2_ + (width / 2)
		y1 = - y1_ + (height / 2)
		y2 = - y2_ + (height / 2)
	lineWin w x1 y1 x2 y2
	addExposeAction w l $ \w' buf -> do
		(x1', y1') <- convertPos w' x1_ y1_
		(x2', y2') <- convertPos w' x2_ y2_
		if buf	then lineUndoBuf w' x1' y1' x2' y2'
			else lineWin w' x1' y1' x2' y2'

convertPos :: Field -> Double -> Double -> IO (Double, Double)
convertPos w x y = do
	(width, height) <- fieldSize w
	return (x + width / 2, - y + height / 2)

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
flushWin = flush . fDisplay

{-
changeColor :: Win -> Pixel -> IO ()
changeColor w = setForeground (fDisplay w) (fGC w)

clearBG :: Win -> IO ()
clearBG w = fieldSizeRaw w >>=
	uncurry (fillRectangle (fDisplay w) (fBG w) (fGCWhite w) 0 0)
-}
