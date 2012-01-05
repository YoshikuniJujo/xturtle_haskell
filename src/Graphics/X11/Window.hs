module Graphics.X11.Window (
	Win,
	openWin,
	flushWin,
	winSize,

	clearBG,
	clearUndoBuf,
	lineBG,
	lineUndoBuf,
	fillPolygonBuf,

	undoBufToBG,
	bgToBuf,
	bufToWin
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
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.Bits((.|.))
import Data.Convertible(convert)

data Win = Win{
	wDisplay :: Display,
	wWindow :: Window,
	wGC :: GC,
	wDel :: Atom,
	wUndoBuf :: Pixmap,
	wBG :: Pixmap,
	wBuf :: Pixmap,
	wWidth :: IORef Dimension,
	wHeight :: IORef Dimension
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
	gc' <- createGC dpy win
	setForeground dpy gc' 0xffffff
	fillRectangle dpy bg gc' 0 0 rWidth rHeight
	fillRectangle dpy buf gc' 0 0 rWidth rHeight
	fillRectangle dpy undoBuf gc' 0 0 rWidth rHeight
	setWMProtocols dpy win [del]
	selectInput dpy win $ exposureMask .|. keyPressMask
	mapWindow dpy win
	widthRef <- newIORef rWidth
	heightRef <- newIORef rHeight
	let w = Win dpy win gc del undoBuf bg buf widthRef heightRef
	exposeAction <- newIORef $ return ()
	_ <- forkIO $ (>> closeDisplay dpy) $ (initThreads >>) $ withEvent w $ \ev ->
		case ev of
			ExposeEvent{} -> do
				(_, _, _, width, height, _, _) <- getGeometry (wDisplay w) (wWindow w)
				writeIORef (wWidth w) width
				writeIORef (wHeight w) height
				join $ readIORef exposeAction
				return True
			KeyEvent{} -> return True
			ClientMessageEvent{} ->
				return $ not $ isDeleteEvent w ev
			_ -> return True
	return (w, exposeAction)

winSize :: Win -> IO (Double, Double)
winSize w = fmap (fromIntegral *** fromIntegral) $ winSizeRaw w

winSizeRaw :: Win -> IO (Dimension, Dimension)
winSizeRaw w = do
	width <- readIORef $ wWidth w
	height <- readIORef $ wHeight w
	return (width, height)

bgToBuf :: Win -> IO ()
bgToBuf w = do
	(width, height) <- winSizeRaw w
	copyArea (wDisplay w) (wBG w) (wBuf w) (wGC w)
		0 0 width height 0 0

bufToWin :: Win -> IO ()
bufToWin w = do
	(width, height) <- winSizeRaw w
	copyArea (wDisplay w) (wBuf w) (wWindow w) (wGC w)
		0 0 width height 0 0

undoBufToBG :: Win -> IO ()
undoBufToBG w = do
	(width, height) <- winSizeRaw w
	copyArea (wDisplay w) (wUndoBuf w) (wBG w) (wGC w) 0 0 width height 0 0

withEvent :: Win -> (Event -> IO Bool) -> IO ()
withEvent w act = doWhile_ $ allocaXEvent $ \e -> do
	nextEvent (wDisplay w) e
	getEvent e >>= act

isDeleteEvent :: Win -> Event -> Bool
isDeleteEvent w ev@ClientMessageEvent{} = convert (head $ ev_data ev) == wDel w
isDeleteEvent _ _ = False

fillPolygonBuf :: Win -> [(Double, Double)] -> IO ()
fillPolygonBuf w ps =
	fillPolygon (wDisplay w) (wBuf w) (wGC w) (mkPs ps) nonconvex coordModeOrigin
	where
	doublesToPoint (x, y) = Point (round x) (round y)
	mkPs = map doublesToPoint

data Buf = BG | UndoBuf

lineBG :: Win -> Double -> Double -> Double -> Double -> IO ()
lineBG w = lineToGen w BG

lineUndoBuf :: Win -> Double -> Double -> Double -> Double -> IO ()
lineUndoBuf w = lineToGen w UndoBuf

lineToGen :: Win -> Buf -> Double -> Double -> Double -> Double -> IO ()
lineToGen w BG x1_ y1_ x2_ y2_ = drawLine (wDisplay w) (wBG w) (wGC w) x1 y1 x2 y2
	where	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]
lineToGen w UndoBuf x1_ y1_ x2_ y2_ = drawLine (wDisplay w) (wUndoBuf w) (wGC w) x1 y1 x2 y2
	where	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]

clearBG :: Win -> IO ()
clearBG w = cleanGen w BG

clearUndoBuf :: Win -> IO ()
clearUndoBuf w = cleanGen w UndoBuf

cleanGen :: Win -> Buf -> IO ()
cleanGen w b = do
	gc <- createGC (wDisplay w) (wWindow w)
	setForeground (wDisplay w) gc 0xffffff
	let buf = case b of
		BG -> wBG w
		UndoBuf -> wUndoBuf w
	winSizeRaw w >>= uncurry (fillRectangle (wDisplay w) buf gc 0 0)

flushWin :: Win -> IO ()
flushWin = flush . wDisplay
