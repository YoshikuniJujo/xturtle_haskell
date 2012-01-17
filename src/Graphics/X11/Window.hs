module Graphics.X11.Window (
	Win,
	openWin,
	flushWin,
	winSize,

	clearUndoBuf,
	lineUndoBuf,
	clearBG,
	lineBG,
	fillPolygonBuf,

	undoBufToBG,
	bgToBuf,
	bufToWin,

	addExposeAction
) where

import Graphics.X11(
	Display, Window, Pixmap, GC, Atom, Point(..), Dimension,

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
import Control.Monad(join)
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
	wExpose :: IORef (IO ())
 }

openWin :: IO (Win, IORef (IO ()))
openWin = do
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
	exposeAction <- newIORef $ return ()
	let w = Win dpy win gc gcWhite del undoBuf bg buf widthRef heightRef exposeAction
	_ <- forkIO $ (>> closeDisplay dpy) $ (initThreads >>) $ withEvent w $ \ev ->
		case ev of
			ExposeEvent{} -> do
				(_, _, _, width, height, _, _) <-
					getGeometry (wDisplay w) (wWindow w)
				writeIORef (wWidth w) width
				writeIORef (wHeight w) height
				join $ readIORef exposeAction
				return True
			KeyEvent{} -> return True
			ClientMessageEvent{} ->
				return $ not $ isDeleteEvent w ev
			_ -> return True
	return (w, exposeAction)
	where
	withEvent w act = doWhile_ $ allocaXEvent $ \e -> do
		nextEvent (wDisplay w) e
		getEvent e >>= act
	isDeleteEvent w ev@ClientMessageEvent{} =
		convert (head $ ev_data ev) == wDel w
	isDeleteEvent _ _ = False

addExposeAction :: Win -> (Win -> IO ()) -> IO ()
addExposeAction w@Win{wExpose = we} act =
	modifyIORef we (>> act w)

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
fillPolygonBuf w ps = fillPolygon (wDisplay w) (wBuf w) (wGC w) (map dtp ps)
						nonconvex coordModeOrigin
	where
	dtp (x, y) = Point (round x) (round y)

lineBG :: Win -> Double -> Double -> Double -> Double -> IO ()
lineBG w x1_ y1_ x2_ y2_ = drawLine (wDisplay w) (wBG w) (wGC w) x1 y1 x2 y2
	where	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]

lineUndoBuf :: Win -> Double -> Double -> Double -> Double -> IO ()
lineUndoBuf w x1_ y1_ x2_ y2_ =
	drawLine (wDisplay w) (wUndoBuf w) (wGC w) x1 y1 x2 y2
	where	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]

clearBG :: Win -> IO ()
clearBG w = winSizeRaw w >>=
	uncurry (fillRectangle (wDisplay w) (wBG w) (wGCWhite w) 0 0)

clearUndoBuf :: Win -> IO ()
clearUndoBuf w = winSizeRaw w >>=
	uncurry (fillRectangle (wDisplay w) (wUndoBuf w) (wGCWhite w) 0 0)

flushWin :: Win -> IO ()
flushWin = flush . wDisplay
