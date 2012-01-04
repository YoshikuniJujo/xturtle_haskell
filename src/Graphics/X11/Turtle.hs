module Graphics.X11.Turtle (
	initTurtle,
	closeTurtle,
	shapesize,
	goto,
	forward,
	backward,
	left,
	right,
	penup,
	pendown,
	isdown,
	home,
	circle,
	clear,
	undoAll,
	undo,
	distance,

	Position,
	testModuleTurtle
) where

import Graphics.X11.World
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

distance :: Double -> Double -> IO Double
distance x0 y0 = do
	(x, y) <- position
	return $ ((x - x0) ** 2 + (y - y0) ** 2) ** (1 / 2)

pastDrawLines :: IORef [Maybe (((Double, Double), Double), Buf -> IO ())]
pastDrawLines = unsafePerformIO $ newIORef []

putToPastDrawLines :: (Double, Double) -> Double -> (Buf -> IO ()) -> IO ()
putToPastDrawLines tpos tdir dl = do
	pdls <- readIORef pastDrawLines
	if length pdls < 300
		then do
			writeIORef pastDrawLines $ pdls ++ [Just ((tpos, tdir), dl)]
		else do
			maybe (return ()) (($ UndoBuf) . snd) $ head pdls
			writeIORef pastDrawLines $ tail pdls ++ [Just ((tpos, tdir), dl)]

setUndoPoint :: IO ()
setUndoPoint = modifyIORef pastDrawLines $ (++ [Nothing])

data PenState = PenUp | PenDown

data Order = Forward Double | Left Double | Undo deriving Show

doesPenDown :: PenState -> Bool
doesPenDown PenUp = False
doesPenDown PenDown = True

penState :: IORef PenState
penState = unsafePerformIO $ newIORef PenDown

testModuleTurtle :: IO ()
testModuleTurtle = main

main :: IO ()
main = do
	putStrLn "module Turtle"
	initTurtle
	w <- readIORef world
	redrawLines
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

orderChan :: IORef (Chan Order)
orderChan = unsafePerformIO $ newChan >>= newIORef

getOrders :: IO [Order]
getOrders = unsafeInterleaveIO $ do
	o <- readIORef orderChan >>= readChan
	os <- getOrders
	return $ o : os

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

shapesize :: Double -> IO ()
shapesize s = do
	w <- readIORef world
	setCursorSize w s
	drawWorld w
	flushWorld w

goto, rawGoto :: Double -> Double -> IO ()
goto x y = do
	width <- windowWidth
	height <- windowHeight
	rawGoto (x + width / 2) (- y + height / 2)
		>> setUndoPoint
rawGoto xTo yTo = do
	w <- readIORef world
	(x0, y0) <- getCursorPos w
	let	step = 10
		distX = xTo - x0
		distY = yTo - y0
		dist = (distX ** 2 + distY ** 2) ** (1 / 2)
		dx = step * distX / dist
		dy = step * distY / dist
	(x', y') <- if (dist <= step) then return (x0, y0) else doWhile (x0, y0) $ \(x, y) -> do
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
forward len = rawForward len >> setUndoPoint
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

penup :: IO ()
penup = writeIORef penState PenUp

pendown :: IO ()
pendown = writeIORef penState PenDown

isdown :: IO Bool
isdown = do
	ps <- readIORef penState
	return $ case ps of
		PenUp -> False
		PenDown -> True

drawLine :: World -> Double -> Double -> Double -> Double -> IO ()
drawLine w x1 y1 x2 y2 = do
	let act buf = do
		ps <- readIORef penState
		when (doesPenDown ps) $
			lineToBG w buf (round x1) (round y1) (round x2) (round y2)
	act BG
	dir <- readIORef world >>= getCursorDir 
	putToPastDrawLines (x2, y2) dir act

redrawLines :: IO ()
redrawLines = do
	w <- readIORef world
	dls <- fmap (map fromJust . filter isJust) $ readIORef pastDrawLines
	clear
	flip mapM_ dls $ \dl -> do
		snd dl BG
		drawWorld w
		flushWorld w
		threadDelay 20000

undoAll :: IO ()
undoAll = do
	w <- readIORef world
	dls <- fmap (map fromJust . filter isJust) $ readIORef pastDrawLines
	flip mapM_ (zip (reverse $ map fst dls) $ map sequence_ $ reverse $ inits
		$ map (($ BG) . snd) dls)
		$ \((pos, dir), dl) -> do
		cleanBG w BG
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
	flip mapM_ (zip (reverse $ map (fst . fromJust) $ filter isJust dls )
		$ map sequence_
		$ map (map (($ BG) . snd)) draw') $ \((pos, dir), dl) -> do
		cleanBG w BG
		undoBufToBG w
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
	putToPastDrawLines pos nd $ const (return ())

rotateTo :: Double -> IO ()
rotateTo d = do
	w <- readIORef world
	d0 <- getCursorDir w
	let	step = 5
		dd = d - d0
	replicateM_ (abs dd `gDiv` step) $
		rotateBy (signum dd * step) >> threadDelay 10000
	setCursorDir w d
	drawWorld w
	flushWorld w

rotate, rawRotate :: Double -> IO ()
rotate d = rawRotate d >> setUndoPoint
rawRotate d = do
	w <- readIORef world
	d0 <- getCursorDir w
	rotateTo $ d0 + d

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
home = goto 0 0 >> rotateTo 0

clear :: IO ()
clear = do
	w <- readIORef world
	cleanBG w BG
	drawWorld w >> flushWorld w
	pos <- getCursorPos w
	dir <- getCursorDir w
	putToPastDrawLines pos dir $ cleanBG w

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
