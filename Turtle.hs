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

kameStat :: IORef (Int, (Position, Position), Double)
kameStat = unsafePerformIO $ newIORef (0, (150, 150), 1)

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

makeTurtle :: Position -> Position -> Double -> Double -> [Point]
makeTurtle x y rad size =
	map (uncurry $ addPoint $ Point x y)
		$ map (rotatePoint rad . mulPoint size) turtle

mulPoint :: Double -> (Position, Position) -> (Position, Position)
mulPoint size (x, y) = (x `mul` size, y `mul` size)

rotatePoint :: Double -> (Position, Position) -> (Position, Position)
rotatePoint rad (x, y) =
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

displayTurtle ::
	(Display, Window, GC) -> Position -> Position -> Int -> Double -> IO ()
displayTurtle xstat x y d s =
	makeFilledPolygon xstat $ makeTurtle x y (fromIntegral d * pi / 180) s

setForegroundColor :: (Display, Window, GC) -> Pixel -> IO ()
setForegroundColor (dpy, _, gc) clr = setForeground dpy gc clr

main = do
	xstat@(dpy, _, _) <- makeWindow
	doWhile (0, 150) $ \(b, x) -> allocaXEvent $ \e -> do
		nextEvent dpy e
		ev <- getEvent e
		case ev of
			ExposeEvent {} -> do
				displayTurtle xstat 150 150 0 0.4
				return ((b, x), True)
			KeyEvent {} -> do
				ch <- getKeyChar xstat ev
				ns <- case ch of
					't' -> fmap (flip (,) x)
						$ kameTurn xstat b x 0.4
					' ' -> fmap ((,) b)
						$ kameForward xstat b (x, 150) 100 0.4
					_ -> return (b, x)
				return (ns, ch /= 'q')
	closeWindow xstat

forward :: Position -> IO ()
forward dx = do
	xstat <- readIORef xstatRef
	(b, (x, y), s) <- readIORef kameStat
	x' <- kameForward xstat b (x, 150) dx $ s * 0.3
	writeIORef kameStat (b, (x', y), s)

turn :: IO ()
turn = do
	xstat <- readIORef xstatRef
	(b, (x, y), s) <- readIORef kameStat
	b' <- kameTurn xstat b x $ s * 0.3
	writeIORef kameStat (b', (x, y), s)

shapeSize :: Double -> IO ()
shapeSize ns = do
	xstat@(dpy, _, _) <- readIORef xstatRef
	(b, (x, y), s) <- readIORef kameStat
	setForegroundColor xstat 0xffffff
	displayTurtle xstat x 150 b $ s * 0.3
	setForegroundColor xstat 0x000000
	displayTurtle xstat x 150 b $ ns * 0.3
	flush dpy
	writeIORef kameStat (b, (x, y), ns)

kameForward ::
	(Display, Window, GC) -> Int -> (Position, Position) -> Position -> Double -> IO Position
kameForward xstat@(dpy, _, _) d0 (x0, y0) dx s = do
	doWhile x0 $ \x -> do
		let nx = x + if d0 == 0 then 10 else - 10
		setForegroundColor xstat 0xffffff
		displayTurtle xstat (x - if d0 == 0 then 10 else - 10) 150
			(if d0 == 0 then 0 else 180) s
		setForegroundColor xstat 0x000000
		displayTurtle xstat x 150 (if d0 == 0 then 0 else 180) s
		flush dpy
		threadDelay 30000
		return (x + if d0 == 0 then 10 else - 10,
			if d0 == 0 then x < x0 + dx else x > x0 - dx)
	return $ (x0 + if d0 == 0 then dx else - dx)

kameTurn :: (Display, Window, GC) -> Int -> Position -> Double -> IO Int
kameTurn xstat@(dpy, _, _) d0 x s = do
	doWhile d0 $ \d -> do
		setForegroundColor xstat 0xffffff
		displayTurtle xstat x 150 (d - 10) s
		setForegroundColor xstat 0x000000
		displayTurtle xstat x 150 d s
		flush dpy
		threadDelay 30000
		return (d + 10, d < d0 + 180)
	return $ if d0 == 0 then 180 else 0

getKeyChar :: (Display, Window, GC) -> Event -> IO Char
getKeyChar (dpy, _, _) ev = do
	ks <- keycodeToKeysym dpy (ev_keycode ev) 0
	return $ chr $ fromEnum ks
