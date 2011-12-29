module Turtle (
	initTurtle,
	forward,
	backward,
	left,
	right,
	penUp,
	penDown,
	clean,
	undoAll,

	Position
) where

import World
import Data.IORef
import Data.List
import System.IO.Unsafe
import Control.Arrow (second)
import Control.Monad.Tools
import Control.Monad
import Control.Concurrent

world :: IORef World
world = unsafePerformIO $ newIORef undefined

pastDrawLines :: IORef [IO ()]
pastDrawLines = unsafePerformIO $ newIORef []

data PenState = PenUp | PenDown

doesPenDown :: PenState -> Bool
doesPenDown PenUp = False
doesPenDown PenDown = True

penState :: IORef PenState
penState = unsafePerformIO $ newIORef PenDown

main :: IO ()
main = do
	putStrLn "module Turtle"
	initTurtle
	w <- readIORef world
	withEvent w () $ \() ev ->
		case ev of
			ExposeEvent{} -> do
				drawWorld w
				return ((), True)
			KeyEvent{} -> do
				ch <- eventToChar w ev
				return ((), (ch /= 'q'))
			ClientMessageEvent{} ->
				return ((), not $ isDeleteEvent w ev)
			AnyEvent{ev_event_type = 14} ->
				return ((), True)
			_ -> error $ "not implemented for event " ++ show ev
	closeTurtle

initTurtle :: IO ()
initTurtle = do
	w <- openWorld
	setCursorPos w 100 200
	setCursorDir w 0
	setCursorSize w 2
	setCursorShape w displayTurtle
	drawWorld w
	flushWorld w
	writeIORef world w

shapeSize :: Double -> IO ()
shapeSize s = do
	w <- readIORef world
	setCursorSize w s
	drawWorld w
	flushWorld w

forward :: Position -> IO ()
forward len = do
	w <- readIORef world
	(x_, y_) <- getCursorPos w
	d <- getCursorDir w
	let	step = signum (fromIntegral len) * 10 :: Double
		x0 = fromIntegral x_ :: Double
		y0 = fromIntegral y_ :: Double
		rad = fromIntegral d * pi / 180
		dx = step * cos rad
		dy = step * sin rad
	(x', y') <- doWhile (x0, y0) $ \(x, y) -> do
		let	nx = x + dx
			ny = y + dy
		setCursorPos w (round x) (round y)
--		lineToBG w (round x) (round y) (round nx) (round ny)
		drawLine w x y nx ny
		drawWorld w
		flushWorld w
		threadDelay 20000
		return ((nx, ny),
			(nx - x0) ** 2 + (ny - y0) ** 2 < (fromIntegral len - step) ** 2)
	let	nx' = x0 + fromIntegral len * cos rad
		ny' = y0 + fromIntegral len * sin rad
	setCursorPos w (round nx') (round ny')
--	lineToBG w (round x') (round y') (round nx') (round ny')
	drawLine w x' y' nx' ny'
	drawWorld w
	flushWorld w
	return ()

backward :: Position -> IO ()
backward = forward . negate

penUp :: IO ()
penUp = writeIORef penState PenUp

penDown :: IO ()
penDown = writeIORef penState PenDown

drawLine :: World -> Double -> Double -> Double -> Double -> IO ()
drawLine w x1 y1 x2 y2 = do
	let act = do
		ps <- readIORef penState
		when (doesPenDown ps) $
			lineToBG w (round x1) (round y1) (round x2) (round y2)
	act
	modifyIORef pastDrawLines (++ [act])

redrawLines :: IO ()
redrawLines = do
	dls <- readIORef pastDrawLines
	w <- readIORef world
	clean
	flip mapM_ dls $ \dl -> do
		dl
		drawWorld w
		flushWorld w
		threadDelay 20000

undoAll :: IO ()
undoAll = do
	w <- readIORef world
	dls <- readIORef pastDrawLines
	flip mapM_ (map sequence_ $ reverse $ inits dls) $ \dl -> do
		cleanBG w
		dl
		drawWorld w
		flushWorld w
		threadDelay 20000

rotateBy :: Int -> IO ()
rotateBy dd = do
	w <- readIORef world
	d0 <- getCursorDir w
	let	nd = (d0 + dd) `mod` 360
	setCursorDir w nd
	drawWorld w
	flushWorld w

rotate :: Int -> IO ()
rotate d = do
	let step = 5
	replicateM_ (fromIntegral $ abs d `div` step) $
		rotateBy (signum d * step) >> threadDelay 10000
	rotateBy $ signum d * (abs d `mod` step)

right :: Int -> IO ()
right = rotate

left :: Int -> IO ()
left = rotate . negate

circle :: Position -> IO ()
circle r = replicateM_ 36 $ do
	forward $ round $ (2 * fromIntegral r * pi / 36 :: Double)
	left 10

home :: IO ()
home = do
	w <- readIORef world
	setCursorPos w 100 200
	setCursorDir w 0
	drawWorld w
	flushWorld w

clean :: IO ()
clean = readIORef world >>= \w -> cleanBG w >> drawWorld w >> flushWorld w

closeTurtle :: IO ()
closeTurtle = readIORef world >>= closeWorld

displayTurtle :: World -> Double -> Int -> Position -> Position -> IO ()
displayTurtle w s d x y =
	makeFilledPolygonCursor w $ map (uncurry $ addPoint $ Point x y)
		$ map (rotatePointD d)
		$ map (mulPoint s) turtle

addPoint :: Point -> Position -> Position -> Point
addPoint (Point x y) dx dy = Point (x + dx) (y + dy)

rotatePointD :: Int -> (Position, Position) -> (Position, Position)
rotatePointD = rotatePointR . (* pi) . (/ 180) . fromIntegral

rotatePointR :: Double -> (Position, Position) -> (Position, Position)
rotatePointR rad (x, y) =
	(x `mul` cos rad - y `mul` sin rad, x `mul` sin rad + y `mul` cos rad)

mulPoint :: Double -> (Position, Position) -> (Position, Position)
mulPoint s (x, y) = (x `mul` s, y `mul` s)

mul :: (Integral a, RealFrac b) => a -> b -> a
x `mul` y = round $ fromIntegral x * y

turtle :: [(Position, Position)]
turtle = ttl ++ reverse (map (second negate) ttl)
	where
	ttl = [
		(- 10, 0),
		(- 8, - 3),
		(- 10, - 5),
		(- 7, - 9),
		(- 5, - 6),
		(0, - 8),
		(4, - 7),
		(6, - 10),
		(8, - 7),
		(7, - 5),
		(10, - 2),
		(13, - 3),
		(16, 0)
	 ]
