module Graphics.X11.CharAndBG (
	Field,
	Turtle,
	openField,
	newTurtle,
	goto,
	rotate,
	direction,
	position,
	shape,
	shapesize,
	undo,
	clear,
	setUndoN,
	windowWidth,
	windowHeight,
	penup,
	pendown,
	isdown,

	testModuleCharAndBG
) where

import Graphics.X11.WindowLayers
import Control.Concurrent
import Data.IORef
import Control.Arrow
import Control.Monad

type Field = Win
type Turtle = Square

openField :: IO Field
openField = do
	w <- openWin
	flushWin w
	return w

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
rotate t = rotateSquare t . negate

direction :: Turtle -> IO Double
direction = fmap negate . readIORef . sDir

position :: Turtle -> IO (Double, Double)
position t = do
	(x_, y_) <- readIORef $ sPos t
	(width, height) <- winSize (sWin t)
	return (x_ - width / 2, - y_ + height / 2)

undo, undoGen :: Turtle -> IO ()
undoGen t = do
	rot : rots <- readIORef $ sRotHist t
	writeIORef (sRotHist t) rots
	d <- readIORef $ sDir t
	case rot of
		Just r -> rotateGen t (d - r) >> return ()
		Nothing -> undoSquare (sWin t) t

undo t = do
	n <- readIORef $ sUndoN t
	ns <- readIORef $ sUndoNs t
	case ns of
		n' : ns' -> do
			writeIORef (sUndoN t) n'
			writeIORef (sUndoNs t) ns'
		_ -> writeIORef (sUndoN t) 1
	print n
	replicateM_ n $ undoGen t

setUndoN :: Turtle -> Int -> IO ()
setUndoN t n = do
	n0 <- readIORef $ sUndoN t
	writeIORef (sUndoN t) n
	modifyIORef (sUndoNs t) (n0 :)

clear :: Turtle -> IO ()
clear Square{sWin = w, sLayer = l} = clearLayer w l

windowWidth, windowHeight :: Turtle -> IO Double
windowWidth = fmap fst . winSize . sWin
windowHeight = fmap snd . winSize . sWin

penup, pendown :: Turtle -> IO ()
penup = flip writeIORef False . sPenDown
pendown = flip writeIORef True . sPenDown

isdown :: Turtle -> IO Bool
isdown = readIORef . sPenDown

data Square = Square{
	sLayer :: Layer,
	sChar :: Character,
	sPos :: IORef (Double, Double),
	sHistory :: IORef [(Double, Double)],
	sSize :: IORef Double,
	sDir :: IORef Double,
	sShape :: IORef [(Double, Double)],
	sUndoN :: IORef Int,
	sUndoNs :: IORef [Int],
	sIsRotated :: IORef Bool,
	sRotHist :: IORef [Maybe Double],
	sPenDown :: IORef Bool,
	sWin :: Win
 }

testModuleCharAndBG :: IO ()
testModuleCharAndBG = main

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
	run <- newIORef 1
	runs <- newIORef []
	isr <- newIORef False
	srh <- newIORef []
	rpd <- newIORef True
	return Square{
		sLayer = l,
		sChar = c,
		sPos = p,
		sHistory = h,
		sSize = sr,
		sWin = w,
		sShape = rsh,
		sDir = dr,
		sUndoN = run,
		sUndoNs = runs,
		sIsRotated = isr,
		sRotHist = srh,
		sPenDown = rpd
	 }

shape :: Square -> String -> IO ()
shape s@Square{sShape = rsh} name =
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
stepTime = 10000

stepDir :: Double
stepDir = 5
stepDirTime :: Int
stepDirTime = 10000

getPoints :: Double -> Double -> Double -> Double -> [(Double, Double)]
getPoints x1 y1 x2 y2 = let
	len = ((x2 - x1) ** 2 + (y2 - y1) ** 2) ** (1/2)
	dx = (x2 - x1) * step / len
	dy = (y2 - y1) * step / len in
	zip (takeWhile (before dx x2) [x1, x1 + dx ..])
		(takeWhile (before dy y2) [y1, y1 + dy ..]) ++
			[(x2, y2)]

before :: (Num a, Ord a) => a -> a -> a -> Bool
before d t x = signum d * t >= signum d * x

showAnimation :: Bool -> Win -> Square -> Double -> Double -> Double -> Double -> IO ()
showAnimation pd w s x1 y1 x2 y2 = do
	(size, d, sh) <- getSizeDirShape s
	if pd then setPolygonCharacterAndLine w (sChar s)
				(getShape sh size d x2 y2) (x1, y1) (x2, y2)
		else setPolygonCharacter w (sChar s) (getShape sh size d x2 y2)
	bufToWin w
	flushWin w

getSizeDirShape :: Square -> IO (Double, Double, [(Double, Double)])
getSizeDirShape s = do
	size <- readIORef (sSize s)
	d <- readIORef (sDir s)
	sh <- readIORef (sShape s)
	return (size, d, sh)

showSquare :: Square -> IO ()
showSquare s@Square{sWin = w} = do
	(x, y) <- readIORef $ sPos s
	(size, d, sh) <- getSizeDirShape s
	setPolygonCharacter w (sChar s) (getShape sh size d x y)
	bufToWin w
	flushWin w

moveSquare :: Win -> Square -> Double -> Double -> IO ()
moveSquare w s@Square{sPos = p} x2 y2 = do
	modifyIORef (sRotHist s) (Nothing :)
	writeIORef (sIsRotated s) False
	(x1, y1) <- readIORef p
	modifyIORef (sHistory s) ((x1, y1) :)
	pd <- readIORef $ sPenDown s
	mapM_ (\(x, y) -> showAnimation pd w s x1 y1 x y >> threadDelay stepTime) $
		getPoints x1 y1 x2 y2
	writeIORef p (x2, y2)
	when pd $ line w (sLayer s) x1 y1 x2 y2
{-
	setPolygonCharacter w (sChar s)
		[(x2, y2), (x2 + 10, y2), (x2 + 10, y2 + 10), (x2, y2 + 10)]
-}

getDirections :: Double -> Double -> [Double]
getDirections ds de = takeWhile beforeDir [ds, ds + dd ..] ++ [de]
	where
	sig = signum (de - ds)
	dd = sig * stepDir
	beforeDir x = sig * x < sig * de

setDirSquare :: Square -> Double -> IO ()
setDirSquare s@Square{sDir = dr} d = do
	writeIORef dr d
	showSquare s

rotateSquare :: Square -> Double -> IO ()
rotateSquare s d = do
	d0 <- rotateGen s d
	modifyIORef (sRotHist s) (Just (d - d0) :)
rotateGen :: Square -> Double -> IO Double
rotateGen s@Square{sDir = dr} d = do
	d0 <- readIORef dr
	mapM_ ((>> threadDelay stepDirTime) . setDirSquare s) $ getDirections d0 d
	writeIORef dr (d `modd` 360)
	return d0

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
	mapM_ (\(x, y) -> showAnimation True w s x2 y2 x y >> threadDelay 50000) $
		getPoints x1 y1 x2 y2
	writeIORef (sPos s) p
	writeIORef (sHistory s) ps

getShape ::
	[(Double, Double)] -> Double -> Double -> Double -> Double -> [(Double, Double)]
getShape sh s d x y =
	map (uncurry (addDoubles (x, y)) . rotatePointD d . mulPoint s) sh

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

