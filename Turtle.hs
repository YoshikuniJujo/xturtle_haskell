module Turtle where

import Graphics.X11
import Data.Bits
import Graphics.X11.Xlib.Extras
import Control.Monad.Tools
import Control.Monad
import Data.Char
import Control.Arrow (second)
import Control.Concurrent
import System.IO.Unsafe
import Data.IORef

winBuf :: IORef Pixmap
winBuf = unsafePerformIO $ newIORef undefined

kameStat :: IORef (Int, (Position, Position), Double)
kameStat = unsafePerformIO $ newIORef (0, (150, 150), 1)

drawRef :: IORef (IO ())
drawRef = unsafePerformIO $ newIORef $ return ()

xstatRef :: IORef (Display, Window, GC)
xstatRef = unsafePerformIO $ newIORef (undefined, undefined, undefined)

initTurtle :: IO ()
initTurtle = makeWindow >> forward 0

makeWindow :: IO (Display, Window, GC)
makeWindow = do
	dpy <- openDisplay ""
	let	scr = defaultScreen dpy
		black = blackPixel dpy scr
		white = whitePixel dpy scr
	rootWin <- rootWindow dpy scr
	win <- createSimpleWindow dpy rootWin 0 0 800 800 1 black white
	pm <- createPixmap dpy win 800 800 $ defaultDepth dpy scr
	gc <- createGC dpy win
	setForegroundColor (dpy, win, gc) 0xffffff
	fillRectangle dpy pm gc 0 0 800 800
	setForegroundColor (dpy, win, gc) 0x000000
	selectInput dpy win $ exposureMask .|. keyPressMask
	mapWindow dpy win
	flush dpy
	writeIORef xstatRef (dpy, win, gc)
	writeIORef winBuf pm
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
	doWhile (0, (150, 150)) $ \(b, (x, y)) -> allocaXEvent $ \e -> do
		drawLines <- readIORef drawRef
		nextEvent dpy e
		ev <- getEvent e
		case ev of
			ExposeEvent {} -> do
				drawLines
				displayTurtle xstat x y b 0.4
				return ((b, (x, y)), True)
			KeyEvent {} -> do
				ch <- getKeyChar xstat ev
				ns <- case ch of
					't' -> fmap (flip (,) (x, y))
						$ kameTurn xstat b 30 (x, y) 0.4
					' ' -> fmap ((,) b)
						$ kameForward xstat b (x, y) 50 0.4
					'r' -> fmap (flip (,) (x, y))
						$ kameTurn xstat b 720 (x, y) 1
					_ -> return (b, (x, y))
				return (ns, ch /= 'q')
			_ -> print ev >> return ((b, (x, y)), True)
	closeWindow xstat

forward :: Position -> IO ()
forward dx = do
	xstat <- readIORef xstatRef
	(b, (x, y), s) <- readIORef kameStat
	(x', y') <- kameForward xstat b (x, y) dx $ s * 0.3
	writeIORef kameStat (b, (x', y'), s)

backward :: Position -> IO ()
backward = forward . negate

home :: IO ()
home = do
	(_, _, s) <- readIORef kameStat
	writeIORef kameStat (0, (150, 150), s)
	forward 0

left :: Int -> IO ()
left = turn . negate

right :: Int -> IO ()
right = turn

circle :: Position -> IO ()
circle size = replicateM_ 36 $ forward (round $ fromIntegral size * pi / 18) >> left 10

turn :: Int -> IO ()
turn d = do
	xstat <- readIORef xstatRef
	(b, (x, y), s) <- readIORef kameStat
	b' <- kameTurn xstat b d (x, y) $ s * 0.3
	writeIORef kameStat (b', (x, y), s)

clear :: IO ()
clear = do
	buf <- readIORef winBuf
	(dpy, win, gc) <- readIORef xstatRef
	setForegroundColor (dpy, win, gc) 0xffffff
	fillRectangle dpy buf gc 0 0 800 800
	setForegroundColor (dpy, win, gc) 0x000000
--	clearWindow dpy buf
	copyArea dpy buf win gc 0 0 800 800 0 0
	flush dpy
	writeIORef drawRef $ return ()
	forward 0

shapeSize :: Double -> IO ()
shapeSize ns = do
	buf <- readIORef winBuf
	xstat@(dpy, win, gc) <- readIORef xstatRef
	(b, (x, y), s) <- readIORef kameStat
	setForegroundColor (dpy, buf, gc) 0xffffff
	displayTurtle (dpy, buf, gc) x 150 b $ s * 0.3
	setForegroundColor (dpy, buf, gc) 0x000000
	displayTurtle (dpy, buf, gc) x 150 b $ ns * 0.3
	copyArea dpy buf win gc 0 0 800 800 0 0
	flush dpy
	writeIORef kameStat (b, (x, y), ns)

kameForward ::
	(Display, Window, GC) -> Int -> (Position, Position) -> Position -> Double
		-> IO (Position, Position)
kameForward xstat@(dpy, win, gc) d0 (x0, y0) len s = do
	buf <- readIORef winBuf
	drawLines <- readIORef drawRef
	let	cs = cos $ fromIntegral d0 * pi / 180 :: Double
		sn = sin $ fromIntegral d0 * pi / 180 :: Double
	(x', y') <- if len < 10 then return (fromIntegral x0, fromIntegral y0)
		else doWhile (fromIntegral x0, fromIntegral y0) $ \(x, y) -> do
			let	nx = x + 10 * cs
				ny = y + 10 * sn
			setForegroundColor (dpy, buf, gc) 0xffffff
			displayTurtle (dpy, buf, gc) (round x) (round y) d0 s
			setForegroundColor (dpy, buf, gc) 0x000000
			drawLines
			displayTurtle (dpy, buf, gc) (round nx) (round ny) d0 s
			drawLine dpy buf gc x0 y0 (round nx) (round ny)
			copyArea dpy buf win gc 0 0 800 800 0 0
			flush dpy
			threadDelay 30000
			return ((nx, ny),
				(nx + 10 * cs - fromIntegral x0) ^ 2 +
					(ny + 10 * sn - fromIntegral y0) ^ 2 < (fromIntegral len) ^ 2)
	setForegroundColor (dpy, buf, gc) 0xffffff
	displayTurtle (dpy, buf, gc) (round x') (round y') d0 s
	setForegroundColor (dpy, buf, gc) 0x000000
	drawLines
	displayTurtle (dpy, buf, gc) (x0 + round (fromIntegral len * cs))
		(y0 + round (fromIntegral len * sn)) d0 s
	let draw = drawLine dpy buf gc x0 y0
		(x0 + round (fromIntegral len * cs))
		(y0 + round (fromIntegral len * sn))
	draw
	writeIORef drawRef $ drawLines >> draw
	copyArea dpy buf win gc 0 0 800 800 0 0
	flush dpy
	return $ (x0 + round (fromIntegral len * cs),
		y0 + round (fromIntegral len * sn))

kameTurn ::
	(Display, Window, GC) -> Int -> Int -> (Position, Position) -> Double -> IO Int
kameTurn xstat@(dpy, win, gc) d0 dd (x, y) s = do
	drawLines <- readIORef drawRef
	buf <- readIORef winBuf
	let	nd = d0 + dd
		ten = signum dd * 10
	d' <- doWhile d0 $ \d -> do
		setForegroundColor (dpy, buf, gc) 0xffffff
		displayTurtle (dpy, buf, gc) x y d s
		setForegroundColor (dpy, buf, gc) 0x000000
		displayTurtle (dpy, buf, gc) x y (d + ten) s
		copyArea dpy buf win gc 0 0 800 800 0 0
		drawLines
		flush dpy
		threadDelay 30000
		return (d + ten, if ten > 0 then d + 20 < nd else d - 20 > nd)
	setForegroundColor (dpy, buf, gc) 0xffffff
	displayTurtle (dpy, buf, gc) x y d' s
	setForegroundColor (dpy, buf, gc) 0x000000
	displayTurtle (dpy, buf, gc) x y nd s
	copyArea dpy buf win gc 0 0 800 800 0 0
	flush dpy
	return $ if nd >= 360 then nd `mod` 360 else nd

getKeyChar :: (Display, Window, GC) -> Event -> IO Char
getKeyChar (dpy, _, _) ev = do
	ks <- keycodeToKeysym dpy (ev_keycode ev) 0
	return $ chr $ fromEnum ks
