module Graphics.X11.World (
	World,
	openWorld,
	setCursorPos,
	getCursorPos,
	setCursorDir,
	getCursorDir,
	setCursorSize,
	setCursorShape,
	drawWorld,
	undoBufToBG,
	closeWorld,
	flushWorld,
	makeFilledPolygonCursor,
	lineToBG,
	cleanBG,
	getWindowSize,
	Buf(..),

	testModuleWorld
) where

import Graphics.X11
import Graphics.X11.Xlib.Extras
import Data.IORef
import Data.Bits
import Data.Char
import Data.Convertible
import Control.Monad.Tools

data World = World{
	wDisplay :: Display,
	wWindow :: Window,
	wGC :: GC,
	wDel :: Atom,
	wBG :: Pixmap,
	wBuf :: Pixmap,
	wUndoBuf :: Pixmap,
	wPos :: IORef (Double, Double),
	wDir :: IORef Double,
	wSize :: IORef Double,
	wShape :: IORef (World -> Double -> Double -> Double -> Double -> IO ())
 }

getWindowSize :: World -> IO (Double, Double)
getWindowSize w = do
	(_, _, _, width, height, _, _) <- getGeometry (wDisplay w) (wWindow w)
	return (fromIntegral width, fromIntegral height)

setCursorPos :: World -> Double -> Double -> IO ()
setCursorPos w x y = writeIORef (wPos w) (x, y)

getCursorPos :: World -> IO (Double, Double)
getCursorPos w = readIORef (wPos w)

setCursorDir :: World -> Double -> IO ()
setCursorDir w d = writeIORef (wDir w) d

getCursorDir :: World -> IO Double
getCursorDir w = readIORef (wDir w)

setCursorSize :: World -> Double -> IO ()
setCursorSize w s = writeIORef (wSize w) s

setCursorShape ::
	World -> (World -> Double -> Double -> Double -> Double -> IO ()) -> IO ()
setCursorShape w s = writeIORef (wShape w) s

testModuleWorld :: IO ()
testModuleWorld = main

main :: IO ()
main = do
	putStrLn "module World"
	w <- openWorld
	withEvent w () $ \() ev ->
		case ev of
			ExposeEvent{} -> return ((), True)
			KeyEvent{} -> do
				ch <- eventToChar w ev
				return ((), ch /= 'q')
			ClientMessageEvent{} ->
				return ((), not $ isDeleteEvent w ev)
			_ -> error $ "not implemented for event " ++ show ev
	closeWorld w

openWorld :: IO World
openWorld = do
	dpy <- openDisplay ""
	del <- internAtom dpy "WM_DELETE_WINDOW" True
	let	scr = defaultScreen dpy
	root <- rootWindow dpy scr
	(_, _, _, width, height, _, _) <- getGeometry dpy root
	let	black = blackPixel dpy scr
		white = whitePixel dpy scr
	win <- createSimpleWindow dpy root 0 0 width height 1 black white
	bg <- createPixmap dpy root width height $ defaultDepth dpy scr
	buf <- createPixmap dpy root width height $ defaultDepth dpy scr
	undoBuf <- createPixmap dpy root width height $ defaultDepth dpy scr
	gc <- createGC dpy win
	gc' <- createGC dpy win
	setForeground dpy gc' 0xffffff
	fillRectangle dpy bg gc' 0 0 width height
	fillRectangle dpy buf gc' 0 0 width height
	fillRectangle dpy undoBuf gc' 0 0 width height
	setWMProtocols dpy win [del]
	selectInput dpy win $ exposureMask .|. keyPressMask
	mapWindow dpy win
	flush dpy
	initPos <- newIORef undefined
	initDir <- newIORef undefined
	initSize <- newIORef undefined
	initShape <- newIORef undefined
	return $ World dpy win gc del bg buf undoBuf initPos initDir initSize initShape

closeWorld :: World -> IO ()
closeWorld = closeDisplay . wDisplay

drawWorld :: World -> IO ()
drawWorld w = do
	(_, _, _, width, height, _, _) <- getGeometry (wDisplay w) (wWindow w)
	copyArea (wDisplay w) (wBG w) (wBuf w) (wGC w) 0 0 width height 0 0
	(x, y) <- readIORef $ wPos w
	d <- readIORef $ wDir w
	s <- readIORef $ wSize w
	displayCursor <- readIORef $ wShape w
	displayCursor w s d x y
	copyArea (wDisplay w) (wBuf w) (wWindow w) (wGC w) 0 0 width height 0 0

undoBufToBG :: World -> IO ()
undoBufToBG w = do
	(_, _, _, width, height, _, _) <- getGeometry (wDisplay w) (wWindow w)
	copyArea (wDisplay w) (wUndoBuf w) (wBG w) (wGC w) 0 0 width height 0 0


withEvent :: World -> s -> (s -> Event -> IO (s, Bool)) -> IO s
withEvent w stat0 act = doWhile stat0 $ \stat -> allocaXEvent $ \e -> do
	nextEvent (wDisplay w) e
	getEvent e >>= act stat

eventToChar :: World -> Event -> IO Char
eventToChar w ev =
	fmap (chr . fromEnum) $ keycodeToKeysym (wDisplay w) (ev_keycode ev) 0

isDeleteEvent :: World -> Event -> Bool
isDeleteEvent w ev@ClientMessageEvent{} = convert (head $ ev_data ev) == wDel w
isDeleteEvent _ _ = False

makeFilledPolygonCursor :: World -> [(Double, Double)] -> IO ()
makeFilledPolygonCursor w ps =
	fillPolygon (wDisplay w) (wBuf w) (wGC w) (mkPs ps) nonconvex coordModeOrigin
	where
	doublesToPoint (x, y) = Point (round x) (round y)
	mkPs = map doublesToPoint

data Buf = BG | UndoBuf

lineToBG :: World -> Buf -> Double -> Double -> Double -> Double -> IO ()
lineToBG w BG x1_ y1_ x2_ y2_ = drawLine (wDisplay w) (wBG w) (wGC w) x1 y1 x2 y2
	where	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]
lineToBG w UndoBuf x1_ y1_ x2_ y2_ = drawLine (wDisplay w) (wUndoBuf w) (wGC w) x1 y1 x2 y2
	where	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]

cleanBG :: World -> Buf -> IO ()
cleanBG w b = do
	(_, _, _, width, height, _, _) <- getGeometry (wDisplay w) (wWindow w)
	gc <- createGC (wDisplay w) (wWindow w)
	setForeground (wDisplay w) gc 0xffffff
	let buf = case b of
		BG -> wBG w
		UndoBuf -> wUndoBuf w
	fillRectangle (wDisplay w) buf gc 0 0 width height

flushWorld :: World -> IO ()
flushWorld = flush . wDisplay
