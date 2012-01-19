module CharAndBG where

import WindowLayers
import Control.Monad.Tools
import Control.Concurrent
import Data.IORef
import Control.Arrow

data Square = Square{
	sLayer :: Layer,
	sChar :: Character,
	sPos :: IORef (Double, Double),
	sHistory :: IORef [(Double, Double)],
	sSize :: IORef Double,
	sWin :: Win
 }

main :: IO ()
main = do
	w <- openWin
	s <- newSquare w 2
	s1 <- newSquare w 2
	shapesize s1 1
	moveSquare w s 100 105
	moveSquare w s1 200 30
	moveSquare w s 50 300
	moveSquare w s1 20 30
	moveSquare w s 300 300
	shapesize s1 2
	undoSquare w s
	moveSquare w s 300 400
	undoSquare w s
	moveSquare w s 300 200
	undoSquare w s
	undoSquare w s1
	undoSquare w s
	getLine >> return ()

newSquare :: Win -> Double -> IO Square
newSquare w s = do
	l <- addLayer w
	c <- addCharacter w
	p <- newIORef (0, 0)
	h <- newIORef []
	sr <- newIORef s
	return $ Square{
		sLayer = l,
		sChar = c,
		sPos = p,
		sHistory = h,
		sSize = sr,
		sWin = w
	 }

shapesize :: Square -> Double -> IO ()
shapesize s size = do
	writeIORef (sSize s) size
	p <- readIORef $ sPos s
	uncurry (moveSquare (sWin s) s) p

step :: Double
step = 10

getPoints :: Double -> Double -> Double -> Double -> [(Double, Double)]
getPoints x1 y1 x2 y2 = let
	len = ((x2 - x1) ^ 2 + (y2 - y1) ^ 2) ** (1/2)
	dx = (x2 - x1) * 10 / len
	dy = (y2 - y1) * 10 / len in
	zip (takeWhile (before dx x2) [x1, x1 + dx ..])
		(takeWhile (before dy y2) [y1, y1 + dy ..]) ++
			[(x2, y2)]

before :: (Num a, Ord a) => a -> a -> a -> Bool
before d t x = signum d * t >= signum d * x

-- showAnimation :: Win -> Square -> Double -> Double -> IO ()
showAnimation w s@Square{sPos = p} x1 y1 x2 y2 = do
--	(x1, y1) <- readIORef p
	size <- readIORef (sSize s)
	setPolygonCharacterAndLine w (sChar s) (getTurtle size 0 x2 y2)
--		[(x2, y2), (x2 + 10, y2), (x2 + 10, y2 + 10), (x2, y2 + 10)]
		(x1, y1) (x2, y2)
	bufToWin w
	flushWin w

moveSquare :: Win -> Square -> Double -> Double -> IO ()
moveSquare w s@Square{sPos = p} x2 y2 = do
	(x1, y1) <- readIORef p
	modifyIORef (sHistory s) ((x1, y1) :)
	mapM_ (\(x, y) -> showAnimation w s x1 y1 x y >> threadDelay 50000) $
		getPoints x1 y1 x2 y2
	writeIORef p (x2, y2)
	line w (sLayer s) x1 y1 x2 y2
{-
	setPolygonCharacter w (sChar s)
		[(x2, y2), (x2 + 10, y2), (x2 + 10, y2 + 10), (x2, y2 + 10)]
-}

undoSquare :: Win -> Square -> IO ()
undoSquare w s@Square{sLayer = l} = do
	undoLayer w l
	(x1, y1) <- readIORef $ sPos s
	p@(x2, y2) : ps <- readIORef $ sHistory s
--	moveSquare w s x y
--	showAnimation w s x1 y1 x y
	mapM_ (\(x, y) -> showAnimation w s x2 y2 x y >> threadDelay 50000) $
		getPoints x1 y1 x2 y2
	writeIORef (sPos s) p
	writeIORef (sHistory s) ps

getTurtle :: Double -> Double -> Double -> Double -> [(Double, Double)]
getTurtle s d x y =
	map (uncurry (addDoubles (x, y)) . rotatePointD d . mulPoint s) turtle

turtle :: [(Double, Double)]
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

addDoubles :: (Double, Double) -> Double -> Double -> (Double, Double)
addDoubles (x, y) dx dy = (x + dx, y + dy)

rotatePointD :: Double -> (Double, Double) -> (Double, Double)
rotatePointD = rotatePointR . (* pi) . (/ 180)

rotatePointR :: Double -> (Double, Double) -> (Double, Double)
rotatePointR rad (x, y) =
	(x * cos rad - y * sin rad, x * sin rad + y * cos rad)

mulPoint :: Double -> (Double, Double) -> (Double, Double)
mulPoint s (x, y) = (x * s, y * s)

