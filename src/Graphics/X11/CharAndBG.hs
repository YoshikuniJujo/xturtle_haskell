module Graphics.X11.CharAndBG (
	Field,
	Turtle,
	openField,
	newTurtle,

	shape,
	shapesize,
	goto,
	rotate,
	undo,
	clear,
	penup,
	pendown,

	windowWidth,
	windowHeight,
	position,
	direction,
	isdown,

	setUndoN,
) where

import Graphics.X11.WindowLayers
import Graphics.X11.SquareState
import Control.Concurrent
import Control.Monad

data Turtle = Turtle{
	sWin :: Field,
	sLayer :: Layer,
	sChar :: Character,
	sStat :: SquareState
 }

newTurtle :: Field -> IO Turtle
newTurtle f = do
	l <- addLayer f
	c <- addCharacter f
	(width, height) <- winSize f
	stat <- getSquareState
	setPosition stat (width / 2) (height / 2)
	let	s =  Turtle{sLayer = l, sChar = c, sWin = f, sStat = stat}
	showSquare s
	return s

goto :: Turtle -> Double -> Double -> IO ()
goto t x y = do
	(width, height) <- winSize (sWin t)
	moveSquare (sWin t) t (x + width / 2) (- y + height / 2)

rotate :: Turtle -> Double -> IO ()
rotate t = rotateSquare t . negate

direction :: Turtle -> IO Double
direction = fmap negate . getDirection . sStat

position :: Turtle -> IO (Double, Double)
position t = do
	(x_, y_) <- getPosition $ sStat t
	(width, height) <- winSize (sWin t)
	return (x_ - width / 2, - y_ + height / 2)

undo, undoGen :: Turtle -> IO ()
undo t = do
	n <- getUndoNum $ sStat t
	n' <- popUndoNum $ sStat t
	setUndoNum (sStat t) n'
	print n
	replicateM_ n $ undoGen t

undoGen t = do
	rot <- popRotHist $ sStat t
	d <- getDirection $ sStat t
	case rot of
		Just r -> rotateGen t (d - r) >> return ()
		Nothing -> undoSquare (sWin t) t

undoSquare :: Field -> Turtle -> IO ()
undoSquare w s@Turtle{sLayer = l} = do
	undoLayer w l
	(x1, y1) <- getPosition $ sStat s
	p@(x2, y2) <- popHistory $ sStat s
	mapM_ (\(x, y) -> showAnimation True w s x2 y2 x y >> threadDelay 50000) $
		getPoints x1 y1 x2 y2
	uncurry (setPosition $ sStat s) p

setUndoN :: Turtle -> Int -> IO ()
setUndoN t n = do
	n0 <- getUndoNum $ sStat t
	setUndoNum (sStat t) n
	pushUndoNum (sStat t) n0

clear :: Turtle -> IO ()
clear Turtle{sWin = w, sLayer = l} = clearLayer w l

windowWidth, windowHeight :: Turtle -> IO Double
windowWidth = fmap fst . winSize . sWin
windowHeight = fmap snd . winSize . sWin

penup, pendown :: Turtle -> IO ()
penup = penUp . sStat
pendown = penDown . sStat

isdown :: Turtle -> IO Bool
isdown = isPenDown . sStat

shape :: Turtle -> String -> IO ()
shape s name = selectShape (sStat s) name >> showSquare s

shapesize :: Turtle -> Double -> IO ()
shapesize s size = setSize (sStat s) size >> showSquare s

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

showAnimation :: Bool -> Field -> Turtle -> Double -> Double -> Double -> Double -> IO ()
showAnimation pd w s x1 y1 x2 y2 = do
	sp <- createShape (sStat s) x2 y2
	if pd then setPolygonCharacterAndLine w (sChar s)
				sp
				(x1, y1) (x2, y2)
		else setPolygonCharacter w (sChar s) sp
	bufToWin w
	flushWin w

showSquare :: Turtle -> IO ()
showSquare s@Turtle{sWin = w} = do
	(x, y) <- getPosition $ sStat s
	sp <- createShape (sStat s) x y
	setPolygonCharacter w (sChar s) sp
	bufToWin w
	flushWin w

moveSquare :: Field -> Turtle -> Double -> Double -> IO ()
moveSquare w s x2 y2 = do
	pushRotHist (sStat s) Nothing
	(x1, y1) <- getPosition $ sStat s
	pushHistory (sStat s) x1 y1
	pd <- isPenDown $ sStat s
	mapM_ (\(x, y) -> showAnimation pd w s x1 y1 x y >> threadDelay stepTime) $
		getPoints x1 y1 x2 y2
	setPosition (sStat s) x2 y2
	when pd $ line w (sLayer s) x1 y1 x2 y2

getDirections :: Double -> Double -> [Double]
getDirections ds de = takeWhile beforeDir [ds, ds + dd ..] ++ [de]
	where
	sig = signum (de - ds)
	dd = sig * stepDir
	beforeDir x = sig * x < sig * de

setDirSquare :: Turtle -> Double -> IO ()
setDirSquare s d = do
	setDirection (sStat s) d
	showSquare s

rotateSquare :: Turtle -> Double -> IO ()
rotateSquare s d = do
	d0 <- rotateGen s d
	pushRotHist (sStat s) $ Just $ d - d0

rotateGen :: Turtle -> Double -> IO Double
rotateGen s d = do
	d0 <- getDirection $ sStat s
	mapM_ ((>> threadDelay stepDirTime) . setDirSquare s) $ getDirections d0 d
	setDirection (sStat s) (d `modd` 360)
	return d0
	where	modd x y	| x < 0 = modd (x + y) y
				| x < y = x
				| otherwise = modd (x - y) y
