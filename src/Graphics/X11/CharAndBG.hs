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
import Graphics.X11.SquareState
import Control.Concurrent
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
direction = fmap negate . getDirection . sStat -- readIORef . sDir . sStat

position :: Turtle -> IO (Double, Double)
position t = do
	(x_, y_) <- getPosition $ sStat t
	(width, height) <- winSize (sWin t)
	return (x_ - width / 2, - y_ + height / 2)

undo, undoGen :: Turtle -> IO ()
undoGen t = do
{-
	rot : rots <- readIORef $ sRotHist $ sStat t
	writeIORef (sRotHist $ sStat t) rots
-}
	rot <- popRotHist $ sStat t
--	d <- readIORef $ sDir $ sStat t
	d <- getDirection $ sStat t
	case rot of
		Just r -> rotateGen t (d - r) >> return ()
		Nothing -> undoSquare (sWin t) t

undo t = do
	n <- getUndoNum $ sStat t
	n' <- popUndoNum $ sStat t
	setUndoNum (sStat t) n'
{-
	ns <- readIORef $ sUndoNs $ sStat t
	case ns of
		n' : ns' -> do
			setUndoNum (sStat t) n'
			writeIORef (sUndoNs $ sStat t) ns'
		_ -> setUndoNum (sStat t) 1 -- writeIORef (sUndoN $ sStat t) 1
-}
	print n
	replicateM_ n $ undoGen t

setUndoN :: Turtle -> Int -> IO ()
setUndoN t n = do
--	n0 <- readIORef $ sUndoN $ sStat t
	n0 <- getUndoNum $ sStat t
--	writeIORef (sUndoN $ sStat t) n
	setUndoNum (sStat t) n
--	modifyIORef (sUndoNs $ sStat t) (n0 :)
	pushUndoNum (sStat t) n0

clear :: Turtle -> IO ()
clear Square{sWin = w, sLayer = l} = clearLayer w l

windowWidth, windowHeight :: Turtle -> IO Double
windowWidth = fmap fst . winSize . sWin
windowHeight = fmap snd . winSize . sWin

penup, pendown :: Turtle -> IO ()
penup = penUp . sStat -- flip writeIORef False . sPenDown . sStat
pendown = penDown . sStat -- flip writeIORef True . sPenDown . sStat

isdown :: Turtle -> IO Bool
isdown = isPenDown . sStat -- readIORef . sPenDown . sStat

data Square = Square{
	sWin :: Win,
	sLayer :: Layer,
	sChar :: Character,
	sStat :: SquareState
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
	stat <- getSquareState
	setPosition stat (width / 2) (height / 2)
	setShape stat classic
	return Square{
		sLayer = l,
		sChar = c,
		sWin = w,
		sStat = stat-- {
--			sShape = rsh,
--			sUndoN = run,
--			sUndoNs = runs,
--			sRotHist = srh,
--			sPenDown = rpd
--		 }
	 }

shape :: Square -> String -> IO ()
shape s name =
	case name of
		"turtle" -> do
			setShape (sStat s) turtle
--			writeIORef (sShape $ sStat s) turtle
			showSquare s
		"clasic" -> do
			setShape (sStat s) classic
--			writeIORef (sShape $ sStat s) classic
			showSquare s
		_ -> return ()

shapesize :: Square -> Double -> IO ()
shapesize s size = do
	setSize (sStat s) size
	p <- getPosition $ sStat s
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
				(mkShape sh size d x2 y2) (x1, y1) (x2, y2)
		else setPolygonCharacter w (sChar s) (mkShape sh size d x2 y2)
	bufToWin w
	flushWin w

getSizeDirShape :: Square -> IO (Double, Double, [(Double, Double)])
getSizeDirShape s = do
	size <- getSize $ sStat s
--	d <- readIORef (sDir $ sStat s)
	d <- getDirection $ sStat s
--	sh <- readIORef (sShape $ sStat s)
	sh <- getShape $ sStat s
	return (size, d, sh)

showSquare :: Square -> IO ()
showSquare s@Square{sWin = w} = do
	(x, y) <- getPosition $ sStat s
	(size, d, sh) <- getSizeDirShape s
	setPolygonCharacter w (sChar s) (mkShape sh size d x y)
	bufToWin w
	flushWin w

moveSquare :: Win -> Square -> Double -> Double -> IO ()
moveSquare w s x2 y2 = do
--	modifyIORef (sRotHist $ sStat s) (Nothing :)
	pushRotHist (sStat s) Nothing
--	writeIORef (sIsRotated $ sStat s) False
	(x1, y1) <- getPosition $ sStat s
--	modifyIORef (sHistory $ sStat s) ((x1, y1) :)
	pushHistory (sStat s) x1 y1
--	pd <- readIORef $ sPenDown $ sStat s
	pd <- isPenDown $ sStat s
	mapM_ (\(x, y) -> showAnimation pd w s x1 y1 x y >> threadDelay stepTime) $
		getPoints x1 y1 x2 y2
	setPosition (sStat s) x2 y2
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
setDirSquare s d = do
--	writeIORef (sDir $ sStat s) d
	setDirection (sStat s) d
	showSquare s

rotateSquare :: Square -> Double -> IO ()
rotateSquare s d = do
	d0 <- rotateGen s d
	pushRotHist (sStat s) $ Just $ d - d0
--	modifyIORef (sRotHist $ sStat s) (Just (d - d0) :)
rotateGen :: Square -> Double -> IO Double
rotateGen s d = do
--	d0 <- readIORef $ sDir $ sStat s
	d0 <- getDirection $ sStat s
	mapM_ ((>> threadDelay stepDirTime) . setDirSquare s) $ getDirections d0 d
--	writeIORef (sDir $ sStat s) (d `modd` 360)
	setDirection (sStat s) (d `modd` 360)
	return d0

modd :: (Num a, Ord a) => a -> a -> a
modd x y
	| x < 0 = modd (x + y) y
	| x < y = x
	| otherwise = modd (x - y) y

undoSquare :: Win -> Square -> IO ()
undoSquare w s@Square{sLayer = l} = do
	undoLayer w l
	(x1, y1) <- getPosition $ sStat s
--	(x1, y1) <- readIORef $ sPos $ sStat s
--	p@(x2, y2) : ps <- readIORef $ sHistory $ sStat s
	p@(x2, y2) <- popHistory $ sStat s
--	moveSquare w s x y
--	showAnimation w s x1 y1 x y
	mapM_ (\(x, y) -> showAnimation True w s x2 y2 x y >> threadDelay 50000) $
		getPoints x1 y1 x2 y2
	uncurry (setPosition $ sStat s) p
--	writeIORef (sPos $ sStat s) p
--	writeIORef (sHistory $ sStat s) ps

mkShape ::
	[(Double, Double)] -> Double -> Double -> Double -> Double -> [(Double, Double)]
mkShape sh s d x y =
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

