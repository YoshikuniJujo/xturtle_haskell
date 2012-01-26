module Graphics.X11.WindowLayers (
	Field,
	Layer,
	Character,

	openField,
	closeField,
	fieldSize,

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
	Window, Pixmap, Atom,

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
import Graphics.X11.Xlib.Types
import Control.Monad.Tools(doWhile_)
import Control.Arrow((***))
import Control.Concurrent(forkIO, ThreadId)
import Data.IORef(IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Bits((.|.))
import Data.Convertible(convert)

data Field = Field{
	wDisplay :: Display,
	wWindow :: Window,
	wGC :: GC,
	wGCWhite :: GC,
	wDel :: Atom,
	wUndoBuf :: Pixmap,
	wBG :: Pixmap,
	wBuf :: Pixmap,
	wWidth :: IORef Dimension,
	wHeight :: IORef Dimension,
	wExpose :: IORef [[Bool -> IO ()]],
	wBuffed :: IORef [IO ()],
	wChars :: IORef [IO ()]
 }

data Layer = Layer Int
data Character = Character Int

closeField :: Field -> IO ()
closeField = closeDisplay . wDisplay

forkIOX :: IO () -> IO ThreadId
forkIOX io = initThreads >> forkIO io

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
	undoBuf <- createPixmap dpy root rWidth rHeight depth
	bg <- createPixmap dpy root rWidth rHeight depth
	buf <- createPixmap dpy root rWidth rHeight depth
	win <- createSimpleWindow dpy root 0 0 rWidth rHeight 1 black white
	gc <- createGC dpy win
	gcWhite <- createGC dpy win
	setForeground dpy gcWhite 0xffffff
	fillRectangle dpy bg gcWhite 0 0 rWidth rHeight
	fillRectangle dpy buf gcWhite 0 0 rWidth rHeight
	fillRectangle dpy undoBuf gcWhite 0 0 rWidth rHeight
	setWMProtocols dpy win [del]
	selectInput dpy win $ exposureMask .|. keyPressMask
	mapWindow dpy win
	widthRef <- newIORef rWidth
	heightRef <- newIORef rHeight
	exposeAction <- newIORef []
	buffedAction <- newIORef []
	charActions <- newIORef []
	let w = Field dpy win gc gcWhite del undoBuf bg buf widthRef heightRef
		exposeAction buffedAction charActions
	_ <- forkIO $ (>> closeDisplay dpy) $ (initThreads >>) $ withEvent w $ \ev ->
		case ev of
			ExposeEvent{} -> do
				(_, _, _, width, height, _, _) <-
					getGeometry (wDisplay w) (wWindow w)
				writeIORef (wWidth w) width
				writeIORef (wHeight w) height
				clearUndoBuf w
				readIORef buffedAction >>= sequence_
				undoBufToBG w
				readIORef exposeAction >>= mapM_ ($ False) . concat
				readIORef charActions >>= sequence_
				bufToWin w
				flushWin w
				return True
			KeyEvent{} -> return True
			ClientMessageEvent{} ->
				return $ not $ isDeleteEvent w ev
			_ -> return True
	flushWin w
	return w
	where
	withEvent w act = doWhile_ $ allocaXEvent $ \e -> do
		nextEvent (wDisplay w) e
		getEvent e >>= act
	isDeleteEvent w ev@ClientMessageEvent{} =
		convert (head $ ev_data ev) == wDel w
	isDeleteEvent _ _ = False

undoN :: Int
undoN = 100

clearLayer :: Field -> Layer -> IO ()
clearLayer w l@(Layer lid) = do
	setExposeAction w l (const $ const $ return ())
	buffed <- readIORef $ wBuffed w
	writeIORef (wBuffed w) $
		take lid buffed ++ [return ()] ++ drop (lid + 1) buffed
	nBuffed <- readIORef $ wBuffed w
	clearUndoBuf w
	sequence_ nBuffed
	undoBufToBG w
	readIORef (wExpose w) >>= mapM_ ($ False) . concat
	bgToBuf w
	readIORef (wChars w) >>= sequence_
	bufToWin w
	flushWin w

addExposeAction :: Field -> Layer -> (Field -> Bool -> IO ()) -> IO ()
addExposeAction w@Field{wExpose = we} (Layer lid) act = do
	ls <- readIORef we
	let	theLayer = ls !! lid
		newLayer = theLayer ++ [act w]
	if length newLayer > undoN
		then do	head newLayer True
			buffed <- readIORef $ wBuffed w
			writeIORef (wBuffed w) $ take lid buffed ++
				[buffed !! lid >> head newLayer True] ++
				drop (lid + 1) buffed
			writeIORef we $ take lid ls ++ [tail newLayer] ++ drop (lid + 1) ls
		else writeIORef we $ take lid ls ++ [newLayer] ++ drop (lid + 1) ls

setExposeAction :: Field -> Layer -> (Field -> Bool -> IO ()) -> IO ()
setExposeAction w@Field{wExpose = we} (Layer lid) act = do
	ls <- readIORef we
	writeIORef we $ take lid ls ++ [[act w]] ++ drop (lid + 1) ls

undoLayer :: Field -> Layer -> IO ()
undoLayer w@Field{wExpose = we} (Layer lid) = do
	ls <- readIORef we
	writeIORef we $ take lid ls ++ [init (ls !! lid)] ++ drop (lid + 1) ls
	undoBufToBG w
	readIORef we >>= mapM_ ($ False) . concat
	bgToBuf w
	readIORef (wChars w) >>= sequence_

setCharacter :: Field -> Character -> IO () -> IO ()
setCharacter w c act = do
	bgToBuf w
	setCharacterAction w c act
	readIORef (wChars w) >>= sequence_

setCharacterAction :: Field -> Character -> IO () -> IO ()
setCharacterAction Field{wChars = wc} (Character cid) act = do
	cs <- readIORef wc
	writeIORef wc $ take cid cs ++ [act] ++ drop (cid + 1) cs

addLayer :: Field -> IO Layer
addLayer Field{wExpose = we, wBuffed = wb} = do
	ls <- readIORef we
	modifyIORef we (++ [[]])
	modifyIORef wb (++ [return ()])
	return $ Layer $ length ls

addCharacter :: Field -> IO Character
addCharacter Field{wChars = wc} = do
	cs <- readIORef wc
	modifyIORef wc (++ [return ()])
	return $ Character $ length cs

fieldSize :: Field -> IO (Double, Double)
fieldSize w = fmap (fromIntegral *** fromIntegral) $ winSizeRaw w

winSizeRaw :: Field -> IO (Dimension, Dimension)
winSizeRaw w = do
	width <- readIORef $ wWidth w
	height <- readIORef $ wHeight w
	return (width, height)

undoBufToBG :: Field -> IO ()
undoBufToBG w = do
	(width, height) <- winSizeRaw w
	copyArea (wDisplay w) (wUndoBuf w) (wBG w) (wGC w) 0 0 width height 0 0

bgToBuf :: Field -> IO ()
bgToBuf w = do
	(width, height) <- winSizeRaw w
	copyArea (wDisplay w) (wBG w) (wBuf w) (wGC w) 0 0 width height 0 0

bufToWin :: Field -> IO ()
bufToWin w = do
	(width, height) <- winSizeRaw w
	copyArea (wDisplay w) (wBuf w) (wWindow w) (wGC w) 0 0 width height 0 0

fillPolygonBuf :: Field -> [(Double, Double)] -> IO ()
fillPolygonBuf w ps = do
	(width, height) <- fieldSize w
	let	dtp (x, y) = Point (round $ x + width / 2) (round $ - y + height / 2)
	fillPolygon (wDisplay w) (wBuf w) (wGC w) (map dtp ps) nonconvex coordModeOrigin

drawCharacter :: Field -> Character -> [(Double, Double)] -> IO ()
drawCharacter w c ps = do
	setCharacter w c (fillPolygonBuf w ps)
	bufToWin w
	flushWin w

drawCharacterAndLine ::
	Field -> Character -> [(Double, Double)] -> (Double, Double) ->
		(Double, Double) -> IO ()
drawCharacterAndLine w c ps (x1, y1) (x2, y2) = do
	setCharacter w c (fillPolygonBuf w ps >> lineBuf w x1 y1 x2 y2)
	bufToWin w
	flushWin w

drawLine :: Field -> Layer -> Double -> Double -> Double -> Double -> IO ()
drawLine w l x1_ y1_ x2_ y2_ = do
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
	X.drawLine (wDisplay w) (wBG w) (wGC w) x1 y1 x2 y2
	bgToBuf w
	readIORef (wChars w) >>= sequence_
	where	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]

lineUndoBuf :: Field -> Double -> Double -> Double -> Double -> IO ()
lineUndoBuf w x1_ y1_ x2_ y2_ =
	X.drawLine (wDisplay w) (wUndoBuf w) (wGC w) x1 y1 x2 y2
	where	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]

lineBuf :: Field -> Double -> Double -> Double -> Double -> IO ()
lineBuf w x1__ y1__ x2__ y2__ = do
	(x1_, y1_) <- convertPos w x1__ y1__
	(x2_, y2_) <- convertPos w x2__ y2__
	let	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]
	X.drawLine (wDisplay w) (wBuf w) (wGC w) x1 y1 x2 y2

clearUndoBuf :: Field -> IO ()
clearUndoBuf w = winSizeRaw w >>=
	uncurry (fillRectangle (wDisplay w) (wUndoBuf w) (wGCWhite w) 0 0)

flushWin :: Field -> IO ()
flushWin = flush . wDisplay

{-
changeColor :: Win -> Pixel -> IO ()
changeColor w = setForeground (wDisplay w) (wGC w)

clearBG :: Win -> IO ()
clearBG w = winSizeRaw w >>=
	uncurry (fillRectangle (wDisplay w) (wBG w) (wGCWhite w) 0 0)
-}
