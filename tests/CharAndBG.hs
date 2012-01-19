module CharAndBG (
	Field,
	Turtle,
	openField,
	newTurtle,
	goto,
	rotate,
	getDirection,
	shape,
	shapesize
) where

import WindowLayers
import Control.Monad.Tools
import Control.Concurrent
import Data.IORef
import Control.Arrow

type Field = Win
type Turtle = Square

openField :: IO Field
openField = openWin

newTurtle :: Field -> IO Turtle
newTurtle f = do
	s <- newSquare f
	showSquare s
	return s

goto :: Turtle -> Double -> Double -> IO ()
goto t x y = do
	(width, height) <- winSize (sWin t)
	moveSquare (sWin t) t (x + width / 2) (- y + height / 2)

rotate :: Turtle -> Double -> IO ()
rotate = rotateSquare

getDirection :: Turtle -> IO Double
getDirection = readIORef . sDir

data Square = Square{
	sLayer :: Layer,
	sChar :: Character,
	sPos :: IORef (Double, Double),
	sHistory :: IORef [(Double, Double)],
	sSize :: IORef Double,
	sDir :: IORef Double,
	sShape :: IORef [(Double, Double)],
	sWin :: Win
 }

main :: IO ()
main = do
	w <- openWin
	s <- newSquare w
	s1 <- newSquare w
	shape s1 "turtle"
	shapesize s1 1
	moveSquare w s 100 105
	moveSquare w s1 200 30
	moveSquare w s 50 300
	moveSquare w s1 20 30
	moveSquare w s 300 300
	shapesize s1 2
	undoSquare w s
	moveSquare w s 300 400
	rotateSquare s1 0
	undoSquare w s
	moveSquare w s 300 200
	undoSquare w s
	undoSquare w s1
	undoSquare w s
	getLine >> return ()

newSquare :: Win -> IO Square
newSquare w = do
	l <- addLayer w
	c <- addCharacter w
	(width, height) <- winSize w
	p <- newIORef (width / 2, height / 2)
	h <- newIORef []
	sr <- newIORef 1
	dr <- newIORef 0
	rsh <- newIORef classic
	return $ Square{
		sLayer = l,
		sChar = c,
		sPos = p,
		sHistory = h,
		sSize = sr,
		sWin = w,
		sShape = rsh,
		sDir = dr
	 }

shape :: Square -> String -> IO ()
shape s@Square{sShape = rsh} name = do
	case name of
		"turtle" -> do
			writeIORef rsh turtle
			showSquare s
		"clasic" -> do
			writeIORef rsh classic
			showSquare s
		_ -> return ()

shapesize :: Square -> Double -> IO ()
shapesize s size = do
	writeIORef (sSize s) size
	p <- readIORef $ sPos s
	uncurry (moveSquare (sWin s) s) p

step :: Double
step = 10
stepTime :: Int
stepTime = 30000

stepDir :: Double
stepDir = 5
stepDirTime :: Int
stepDirTime = 30000

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
	d <- readIORef (sDir s)
	shape <- readIORef (sShape s)
	setPolygonCharacterAndLine w (sChar s) (getShape shape size d x2 y2)
--		[(x2, y2), (x2 + 10, y2), (x2 + 10, y2 + 10), (x2, y2 + 10)]
		(x1, y1) (x2, y2)
	bufToWin w
	flushWin w

showSquare :: Square -> IO ()
showSquare s@Square{sWin = w} = do
	(x, y) <- readIORef $ sPos s
	size <- readIORef (sSize s)
	d <- readIORef (sDir s)
	shape <- readIORef (sShape s)
	setPolygonCharacter w (sChar s) (getShape shape size d x y)
	bufToWin w
	flushWin w

moveSquare :: Win -> Square -> Double -> Double -> IO ()
moveSquare w s@Square{sPos = p} x2 y2 = do
	(x1, y1) <- readIORef p
	modifyIORef (sHistory s) ((x1, y1) :)
	mapM_ (\(x, y) -> showAnimation w s x1 y1 x y >> threadDelay stepTime) $
		getPoints x1 y1 x2 y2
	writeIORef p (x2, y2)
	line w (sLayer s) x1 y1 x2 y2
{-
	setPolygonCharacter w (sChar s)
		[(x2, y2), (x2 + 10, y2), (x2 + 10, y2 + 10), (x2, y2 + 10)]
-}

getDirections :: Double -> Double -> [Double]
getDirections ds de = takeWhile before [ds, ds + dd ..] ++ [de]
	where
	sig = signum (de - ds)
	dd = sig * stepDir
	before x = sig * x < sig * de

setDirSquare :: Square -> Double -> IO ()
setDirSquare s@Square{sDir = dr} d = do
	writeIORef dr d
	showSquare s

rotateSquare :: Square -> Double -> IO ()
rotateSquare s@Square{sDir = dr} d = do
	d0 <- readIORef dr
	mapM_ ((>> threadDelay stepDirTime) . setDirSquare s) $ getDirections d0 d
	writeIORef dr (d `modd` 360)

modd :: (Num a, Ord a) => a -> a -> a
modd x y
	| x < 0 = modd (x + y) y
	| x < y = x
	| otherwise = modd (x - y) y

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

getShape ::
	[(Double, Double)] -> Double -> Double -> Double -> Double -> [(Double, Double)]
getShape shape s d x y =
	map (uncurry (addDoubles (x, y)) . rotatePointD d . mulPoint s) shape

getTurtle :: Double -> Double -> Double -> Double -> [(Double, Double)]
getTurtle s d x y =
	map (uncurry (addDoubles (x, y)) . rotatePointD d . mulPoint s) turtle

classic :: [(Double, Double)]
classic = clssc ++ reverse (map (second negate) clssc)
	where
	clssc = [
		(- 10, 0),
		(- 16, 6),
		(0, 0)
	 ]

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

