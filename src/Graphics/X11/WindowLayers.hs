module Graphics.X11.WindowLayers (
	Field,
	Layer,
	Character,

	openField,
	closeField,
	bufToWin,
	flushWin,
	winSize,

	addLayer,
	addCharacter,

	line,
	setPolygonCharacter,
	setPolygonCharacterAndLine,

	undoLayer,
	clearLayer,
) where

import Graphics.X11(
	Window, Pixmap, Atom,

	openDisplay, closeDisplay, flush, defaultScreen, rootWindow,
	whitePixel, blackPixel,	defaultDepth,
	createSimpleWindow, mapWindow, createPixmap, internAtom, createGC,

	setForeground, copyArea,
	drawLine, fillRectangle, fillPolygon, nonconvex, coordModeOrigin,

	setWMProtocols, selectInput, allocaXEvent, nextEvent,
	keyPressMask, exposureMask,

	getGeometry, initThreads
 )
import Graphics.X11.Xlib.Extras(Event(..), getEvent)
import Graphics.X11.Xlib.Types
import Control.Monad.Tools(doWhile_)
import Control.Arrow((***))
import Control.Concurrent(forkIO)
import Data.IORef(IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Bits((.|.))
import Data.Convertible(convert)

data Win = Win{
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

type Field = Win

openField :: IO Field
openField = openWin

closeField :: Field -> IO ()
closeField = closeDisplay . wDisplay

openWin :: IO Win
openWin = do
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
	let w = Win dpy win gc gcWhite del undoBuf bg buf widthRef heightRef
		exposeAction buffedAction charActions
	_ <- forkIO $ (>> closeDisplay dpy) $ (initThreads >>) $ withEvent w $ \ev ->
		case ev of
			ExposeEvent{} -> do
				(_, _, _, width, height, _, _) <-
					getGeometry (wDisplay w) (wWindow w)
				writeIORef (wWidth w) width
				writeIORef (wHeight w) height
--				clearBG w
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

clearLayer :: Win -> Layer -> IO ()
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

addExposeAction :: Win -> Layer -> (Win -> Bool -> IO ()) -> IO ()
addExposeAction w@Win{wExpose = we} (Layer lid) act = do
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

setExposeAction :: Win -> Layer -> (Win -> Bool -> IO ()) -> IO ()
setExposeAction w@Win{wExpose = we} (Layer lid) act = do
	ls <- readIORef we
	writeIORef we $ take lid ls ++ [[act w]] ++ drop (lid + 1) ls

undoLayer :: Win -> Layer -> IO ()
undoLayer w@Win{wExpose = we} (Layer lid) = do
	ls <- readIORef we
	writeIORef we $ take lid ls ++ [init (ls !! lid)] ++ drop (lid + 1) ls
	undoBufToBG w
	readIORef we >>= mapM_ ($ False) . concat
	bgToBuf w
	readIORef (wChars w) >>= sequence_
--	bufToWin w
--	flushWin w

setCharacter :: Win -> Character -> IO () -> IO ()
setCharacter w c act = do
	bgToBuf w
	setCharacterAction w c act
	readIORef (wChars w) >>= sequence_
--	bufToWin w
--	flushWin w

setCharacterAction :: Win -> Character -> IO () -> IO ()
setCharacterAction Win{wChars = wc} (Character cid) act = do
	cs <- readIORef wc
	writeIORef wc $ take cid cs ++ [act] ++ drop (cid + 1) cs

addLayer :: Win -> IO Layer
addLayer Win{wExpose = we, wBuffed = wb} = do
	ls <- readIORef we
	modifyIORef we (++ [[]])
	modifyIORef wb (++ [return ()])
	return $ Layer $ length ls

addCharacter :: Win -> IO Character
addCharacter Win{wChars = wc} = do
	cs <- readIORef wc
	modifyIORef wc (++ [return ()])
	return $ Character $ length cs

winSize :: Win -> IO (Double, Double)
winSize w = fmap (fromIntegral *** fromIntegral) $ winSizeRaw w

winSizeRaw :: Win -> IO (Dimension, Dimension)
winSizeRaw w = do
	width <- readIORef $ wWidth w
	height <- readIORef $ wHeight w
	return (width, height)

undoBufToBG :: Win -> IO ()
undoBufToBG w = do
	(width, height) <- winSizeRaw w
	copyArea (wDisplay w) (wUndoBuf w) (wBG w) (wGC w) 0 0 width height 0 0

bgToBuf :: Win -> IO ()
bgToBuf w = do
	(width, height) <- winSizeRaw w
	copyArea (wDisplay w) (wBG w) (wBuf w) (wGC w) 0 0 width height 0 0

bufToWin :: Win -> IO ()
bufToWin w = do
	(width, height) <- winSizeRaw w
	copyArea (wDisplay w) (wBuf w) (wWindow w) (wGC w) 0 0 width height 0 0

fillPolygonBuf :: Win -> [(Double, Double)] -> IO ()
fillPolygonBuf w ps = do
	(width, height) <- winSize w
	let	dtp (x, y) = Point (round $ x + width / 2) (round $ - y + height / 2)
	fillPolygon (wDisplay w) (wBuf w) (wGC w) (map dtp ps) nonconvex coordModeOrigin

setPolygonCharacter :: Win -> Character -> [(Double, Double)] -> IO ()
setPolygonCharacter w c ps = setCharacter w c (fillPolygonBuf w ps)

setPolygonCharacterAndLine ::
	Win -> Character -> [(Double, Double)] -> (Double, Double) ->
		(Double, Double) -> IO ()
setPolygonCharacterAndLine w c ps (x1_, y1_) (x2_, y2_) =
	setCharacter w c (fillPolygonBuf w ps >> lineBuf w x1_ y1_ x2_ y2_)

line :: Win -> Layer -> Double -> Double -> Double -> Double -> IO ()
line w l x1_ y1_ x2_ y2_ = do
	(width, height) <- winSize w
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

convertPos :: Win -> Double -> Double -> IO (Double, Double)
convertPos w x y = do
	(width, height) <- winSize w
	return (x + width / 2, - y + height / 2)

lineWin :: Win -> Double -> Double -> Double -> Double -> IO ()
lineWin w x1_ y1_ x2_ y2_ = do
	drawLine (wDisplay w) (wBG w) (wGC w) x1 y1 x2 y2
	bgToBuf w
	readIORef (wChars w) >>= sequence_
--	bufToWin w
	where	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]

lineUndoBuf :: Win -> Double -> Double -> Double -> Double -> IO ()
lineUndoBuf w x1_ y1_ x2_ y2_ =
	drawLine (wDisplay w) (wUndoBuf w) (wGC w) x1 y1 x2 y2
	where	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]

lineBuf :: Win -> Double -> Double -> Double -> Double -> IO ()
lineBuf w x1__ y1__ x2__ y2__ = do
	(x1_, y1_) <- convertPos w x1__ y1__
	(x2_, y2_) <- convertPos w x2__ y2__
	let	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]
	drawLine (wDisplay w) (wBuf w) (wGC w) x1 y1 x2 y2

clearUndoBuf :: Win -> IO ()
clearUndoBuf w = winSizeRaw w >>=
	uncurry (fillRectangle (wDisplay w) (wUndoBuf w) (wGCWhite w) 0 0)

flushWin :: Win -> IO ()
flushWin = flush . wDisplay

{-
changeColor :: Win -> Pixel -> IO ()
changeColor w = setForeground (wDisplay w) (wGC w)

clearBG :: Win -> IO ()
clearBG w = winSizeRaw w >>=
	uncurry (fillRectangle (wDisplay w) (wBG w) (wGCWhite w) 0 0)
-}
