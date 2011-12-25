module Turtle where

import Graphics.X11
import Data.Bits
import Graphics.X11.Xlib.Extras
import Control.Monad.Tools
import Data.Char
import Control.Arrow
import Control.Concurrent
import System.IO.Unsafe
import Data.IORef

kameStat :: IORef (Bool, Position)
kameStat = unsafePerformIO $ newIORef (True, 150)

xstatRef :: IORef (Display, Window, GC)
xstatRef = unsafePerformIO $ newIORef (undefined, undefined, undefined)

makeWindow :: IO (Display, Window, GC)
makeWindow = do
	dpy <- openDisplay ""
	let	scr = defaultScreen dpy
		black = blackPixel dpy scr
		white = whitePixel dpy scr
	rootWin <- rootWindow dpy scr
	win <- createSimpleWindow dpy rootWin 0 0 300 300 1 black white
	gc <- createGC dpy win
	selectInput dpy win $ exposureMask .|. keyPressMask
	mapWindow dpy win
	flush dpy
	writeIORef xstatRef (dpy, win, gc)
	return (dpy, win, gc)

makeFilledPolygon :: (Display, Window, GC) -> [Point] -> IO ()
makeFilledPolygon (dpy, win, gc) ps = do
	fillPolygon dpy win gc ps nonconvex coordModeOrigin
--	flush dpy

closeWindow :: (Display, Window, GC) -> IO ()
closeWindow (dpy, _, _) = closeDisplay dpy

addPoint :: Point -> Position -> Position -> Point
addPoint (Point x y) dx dy = Point (x + dx) (y + dy)

makeTurtle :: Position -> Position -> Double -> [Point]
makeTurtle x y rad =
	map (uncurry $ addPoint $ Point x y)
		$ map (uncurry $ rotatePoint rad) turtle

rotatePoint :: Double -> Position -> Position -> (Position, Position)
rotatePoint rad x y =
	(x `mul` cos rad - y `mul` sin rad, x `mul` sin rad + y `mul` cos rad)

mul :: (Integral a, RealFrac b) => a -> b -> a
mul x y = round $ fromIntegral x * y

turtle :: [(Position, Position)]
turtle = ttl ++ reverse (map (second $ \x -> - x) ttl)
	where
	ttl =[
		(- 50, 0),
		(- 40, - 15),
		(- 50, - 25),
		(- 35, - 45),
		(- 25, - 30),

		(0, - 40),

		(20, - 35),
		(30, - 50),
		(40, - 35),
		(35, - 25),

		(50, - 10),
		(65, - 15),

		(80, 0)
	 ]

reverseY :: Position -> Point -> Point
reverseY y0 (Point x y) = Point x $ 2 * y0 - y

displayTurtle :: (Display, Window, GC) -> Position -> Position -> Int -> IO ()
displayTurtle xstat x y d =
	makeFilledPolygon xstat $ makeTurtle x y $ fromIntegral d * pi / 180

setForegroundColor :: (Display, Window, GC) -> Pixel -> IO ()
setForegroundColor (dpy, _, gc) clr = setForeground dpy gc clr

main = do
	xstat@(dpy, _, _) <- makeWindow
	doWhile (True, 150) $ \(b, x) -> allocaXEvent $ \e -> do
		nextEvent dpy e
		ev <- getEvent e
		case ev of
			ExposeEvent {} -> do
				displayTurtle xstat 150 150 0
				return ((b, x), True)
			KeyEvent {} -> do
				ch <- getKeyChar xstat ev
				ns <- case ch of
					't' -> fmap (flip (,) x) $ kameTurn xstat b x
					' ' -> fmap ((,) b) $ kameForward xstat b x
					_ -> return (b, x)
				return (ns, ch /= 'q')
	closeWindow xstat

forward :: IO ()
forward = do
	xstat <- readIORef xstatRef
	(b, x) <- readIORef kameStat
	x' <- kameForward xstat b x
	writeIORef kameStat (b, x')

turn :: IO ()
turn = do
	xstat <- readIORef xstatRef
	(b, x) <- readIORef kameStat
	b' <- kameTurn xstat b x
	writeIORef kameStat (b', x)

kameForward :: (Display, Window, GC) -> Bool -> Position -> IO Position
kameForward xstat@(dpy, _, _) b x0 = do
	doWhile x0 $ \x -> do
		setForegroundColor xstat 0xffffff
		displayTurtle xstat (x - if b then 10 else - 10) 150 $
			if b then 0 else 180
		setForegroundColor xstat 0x000000
		displayTurtle xstat x 150 $ if b then 0 else 180
		flush dpy
		threadDelay 30000
		return (x + if b then 10 else - 10,
			if b then x < x0 + 100 else x > x0 - 100)
	return $ (x0 + if b then 100 else - 100)

kameTurn :: (Display, Window, GC) -> Bool -> Position -> IO Bool
kameTurn xstat@(dpy, _, _) b x = do
	doWhile (if b then 0 else 180) $ \d -> do
		setForegroundColor xstat 0xffffff
		displayTurtle xstat x 150 $ d - 10
		setForegroundColor xstat 0x000000
		displayTurtle xstat x 150 d
		flush dpy
		threadDelay 30000
		return (d + 10, d < if b then 180 else 360)
	return $ not b

getKeyChar :: (Display, Window, GC) -> Event -> IO Char
getKeyChar (dpy, _, _) ev = do
	ks <- keycodeToKeysym dpy (ev_keycode ev) 0
	return $ chr $ fromEnum ks
