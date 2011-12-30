module Turtle (
	initTurtle,
	forward,
	backward,
	left,
	right,
	penUp,
	penDown,
	clear,
	undoAll,
	undo,

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
import Data.Maybe

world :: IORef World
world = unsafePerformIO $ newIORef undefined

windowWidth :: IO Double
windowWidth = readIORef world >>= fmap fst . getWindowSize

windowHeight :: IO Double
windowHeight = readIORef world >>= fmap snd . getWindowSize

position :: IO (Double, Double)
position = do
	w <- readIORef world
	(x, y) <- getCursorPos w
	width <- windowWidth
	height <- windowHeight
	return (x - width / 2, height / 2 - y)

pastDrawLines :: IORef [Maybe (((Double, Double), Double), IO ())]
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
	width <- windowWidth
	height <- windowHeight
	setCursorPos w (width / 2) (height / 2)
	drawWorld w
	flushWorld w

shapeSize :: Double -> IO ()
shapeSize s = do
	w <- readIORef world
	setCursorSize w s
	drawWorld w
	flushWorld w

goto, rawGoto :: Double -> Double -> IO ()
goto x y = rawGoto x y >> modifyIORef pastDrawLines (++ [Nothing])
rawGoto xTo yTo = do
	w <- readIORef world
	(x0, y0) <- getCursorPos w
	let	step = 10
		distX = xTo - x0
		distY = yTo - y0
		dist = (distX ** 2 + distY ** 2) ** (1 / 2)
		dx = step * distX / dist
		dy = step * distY / dist
	(x', y') <- doWhile (x0, y0) $ \(x, y) -> do
		let	nx = x + dx
			ny = y + dy
		setCursorPos w nx ny
		drawLine w x y nx ny
		drawWorld w
		flushWorld w
		threadDelay 20000
		return ((nx, ny),
			(nx + dx - x0) ** 2 + (ny + dy - y0) ** 2 < dist ** 2)
	setCursorPos w xTo yTo
	drawLine w x' y' xTo yTo
	drawWorld w
	flushWorld w

forward, rawForward :: Double -> IO ()
forward len = rawForward len >> modifyIORef pastDrawLines (++ [Nothing])
rawForward len = do
	w <- readIORef world
	(x0, y0) <- getCursorPos w
	d <- getCursorDir w
	let	rad = d * pi / 180
		nx' = x0 + len * cos rad
		ny' = y0 + len * sin rad
	rawGoto nx' ny'

backward :: Double -> IO ()
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
	dir <- readIORef world >>= getCursorDir 
	modifyIORef pastDrawLines (++ [Just (((x2, y2), dir), act)])

redrawLines :: IO ()
redrawLines = do
	w <- readIORef world
	dls <- fmap (map fromJust . filter isJust) $ readIORef pastDrawLines
	clear
	flip mapM_ dls $ \dl -> do
		snd dl
		drawWorld w
		flushWorld w
		threadDelay 20000

undoAll :: IO ()
undoAll = do
	w <- readIORef world
	dls <- fmap (map fromJust . filter isJust) $ readIORef pastDrawLines
	flip mapM_ (zip (reverse $ map fst dls) $ map sequence_ $ reverse $ inits $ map snd dls)
		$ \((pos, dir), dl) -> do
		cleanBG w
		dl
		uncurry (setCursorPos w) pos
		setCursorDir w dir
		drawWorld w
		flushWorld w
		threadDelay 20000

undo :: IO ()
undo = do
	w <- readIORef world
	dls <- fmap init $ readIORef pastDrawLines
	let	draw = map (map fromJust . filter isJust)
			$ takeWhile (isJust . last) $ reverse $ inits dls
		draw1 = map fromJust $ filter isJust $ head
			$ dropWhile (isJust . last) $ reverse $ inits dls
		draw' = draw ++ [draw1]
	flip mapM_ (zip ({-tail $-} reverse $ map (fst . fromJust) $ filter isJust dls ) $ map sequence_
		$ map (map snd) draw') $ \((pos, dir), dl) -> do
		cleanBG w
		dl
		uncurry (setCursorPos w) pos
		setCursorDir w dir
		drawWorld w
		flushWorld w
		threadDelay 20000
	modifyIORef pastDrawLines $ reverse . dropWhile isJust . tail . reverse

rotateBy :: Double -> IO ()
rotateBy dd = do
	w <- readIORef world
	d0 <- getCursorDir w
	let	nd = (d0 + dd) `gMod` 360
	setCursorDir w nd
	drawWorld w
	flushWorld w
	pos <- getCursorPos w
	modifyIORef pastDrawLines (++ [Just ((pos, nd), return ())])

rotate, rawRotate :: Double -> IO ()
rotate d = rawRotate d >> modifyIORef pastDrawLines (++ [Nothing])
rawRotate d = do
	let step = 5
	replicateM_ (abs d `gDiv` step) $
		rotateBy (signum d * step) >> threadDelay 10000
	rotateBy $ signum d * (abs d `gMod` step)

gDiv :: (Num a, Ord a, Integral b) => a -> a -> b
x `gDiv` y
	| x >= y = 1 + (x - y) `gDiv` y
	| otherwise = 0

gMod :: (Num a, Ord a) => a -> a -> a
x `gMod` y
	| x >= y = (x - y) `gMod` y
	| otherwise = x

right :: Double -> IO ()
right = rotate

left :: Double -> IO ()
left = rotate . negate

circle :: Position -> IO ()
circle r = replicateM_ 36 $ do
	rawForward $ (2 * fromIntegral r * pi / 36 :: Double)
	rawRotate (- 10)

home :: IO ()
home = do
	w <- readIORef world
	width <- windowWidth
	height <- windowHeight
	setCursorPos w (width / 2) (height / 2)
	setCursorDir w 0
	drawWorld w
	flushWorld w
{-
	setCursorPos w 100 200
	setCursorDir w 0
	drawWorld w
	flushWorld w
-}

clear :: IO ()
clear = do
	w <- readIORef world
	cleanBG w
	drawWorld w >> flushWorld w
	pos <- getCursorPos w
	dir <- getCursorDir w
	modifyIORef pastDrawLines (++ [Just ((pos, dir), cleanBG w)])

closeTurtle :: IO ()
closeTurtle = readIORef world >>= closeWorld

displayTurtle :: World -> Double -> Double -> Double -> Double -> IO ()
displayTurtle w s d x y =
	makeFilledPolygonCursor w $ map (uncurry $ addPoint $ Point (round x) (round y))
		$ map (rotatePointD d)
		$ map (mulPoint s) turtle

addPoint :: Point -> Position -> Position -> Point
addPoint (Point x y) dx dy = Point (x + dx) (y + dy)

rotatePointD :: Double -> (Position, Position) -> (Position, Position)
rotatePointD = rotatePointR . (* pi) . (/ 180)

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
