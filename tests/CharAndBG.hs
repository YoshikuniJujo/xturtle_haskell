module CharAndBG (
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

import WindowLayers
import SquareState
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
	stat <- getSquareState
	let	s =  Turtle{sLayer = l, sChar = c, sWin = f, sStat = stat}
	(width, height) <- windowSize s
	setPos stat (width / 2) (height / 2)
	showSquare s
	return s

shape :: Turtle -> String -> IO ()
shape s = (>> showSquare s) . selectShape (sStat s)

shapesize :: Turtle -> Double -> IO ()
shapesize s = (>> showSquare s) . setSize (sStat s)

goto :: Turtle -> Double -> Double -> IO ()
goto s xTo_ yTo_ = do
	(xTo, yTo) <- convertPosition xTo_ yTo_
	pushRotHist (sStat s) Nothing
	(x0, y0) <- getPos s
	setPos (sStat s) xTo yTo
	pd <- isdown s
	let pos0 = if pd then Just (x0, y0) else Nothing
	forM_ (getPoints x0 y0 xTo yTo) $ \(x, y) -> do
		drawTurtle s (x, y) pos0
		threadDelay stepTime
	when pd $ do
		line (sWin s) (sLayer s) x0 y0 xTo yTo
		lineDone $ sStat s
	where
	convertPosition x y = do
		(width, height) <- windowSize s
		return (x + width / 2, -y + height / 2)

getPos :: Turtle -> IO (Double, Double)
getPos = getPosition . sStat

rotate :: Turtle -> Double -> IO ()
rotate t = rotateSquare t . negate

direction :: Turtle -> IO Double
direction = fmap negate . getDirection . sStat

position :: Turtle -> IO (Double, Double)
position t = do
	(x_, y_) <- getPosition $ sStat t
	(width, height) <- windowSize t
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
	(x1, y1) <- popPos $ sStat s
	(x2, y2) <- getPosition $ sStat s
	mapM_ (\(x, y) -> showAnimation True s x2 y2 x y >> threadDelay 50000) $
		getPoints x1 y1 x2 y2

setUndoN :: Turtle -> Int -> IO ()
setUndoN t n = do
	n0 <- getUndoNum $ sStat t
	setUndoNum (sStat t) n
	pushUndoNum (sStat t) n0

clear :: Turtle -> IO ()
clear Turtle{sWin = w, sLayer = l} = clearLayer w l

windowSize :: Turtle -> IO (Double, Double)
windowSize = winSize . sWin

windowWidth, windowHeight :: Turtle -> IO Double
windowWidth = fmap fst . windowSize
windowHeight = fmap snd . windowSize

penup, pendown :: Turtle -> IO ()
penup = penUp . sStat
pendown = penDown . sStat

isdown :: Turtle -> IO Bool
isdown = isPenDown . sStat

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

showAnimation :: Bool -> Turtle -> Double -> Double -> Double -> Double -> IO ()
showAnimation pd s x1 y1 x2 y2 =
	if pd then drawTurtle s (x2, y2) $ Just (x1, y1)
		else drawTurtle s (x2, y2) Nothing

showSquare :: Turtle -> IO ()
showSquare s = do
	p <- getPosition $ sStat s
	drawTurtle s p Nothing

drawTurtle :: Turtle -> (Double, Double) -> Maybe (Double, Double) -> IO ()
drawTurtle t@Turtle{sWin = w, sChar = c} (x, y) org = do
	sp <- createShape (sStat t) x y
	maybe (setPolygonCharacter w c sp)
		(flip (setPolygonCharacterAndLine w c sp) (x, y)) org
	bufToWin w
	flushWin w

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
