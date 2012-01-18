module Graphics.X11.TurtleBase (
	Turtle,
	Buf(..),

	openWorld,
	initTurtle,

	windowWidth,
	windowHeight,
	position,
	getPosition,
	getDirection,
	isdown,

	shapesize,
	drawLine,
	moveTurtle,
	rotateBy,
	clear,
	penup,
	pendown,
	setDirection,
	setPosition,

	initUndo,
	flushW
) where

import Graphics.X11.World
import Control.Arrow
import Data.IORef
import Control.Monad
import Control.Concurrent

data Turtle = Turtle{
	tWorld :: World,
	tPos :: IORef (Double, Double),
	tDir :: IORef Double,
	tSize :: IORef Double,
	tShape :: IORef (World -> Double -> Double -> Double -> Double -> IO ()),
	tPenState :: IORef PenState
 }

drawTurtle :: Turtle -> IO ()
drawTurtle t = drawShape (tShape t) (tPos t) (tDir t) (tSize t) (tWorld t)

drawShape :: IORef (World -> Double -> Double -> Double -> Double -> IO ()) ->
	IORef (Double, Double) -> IORef Double -> IORef Double -> World -> IO ()
drawShape rshape rpos rd rs w = do
	(x, y) <- readIORef rpos
	d <- readIORef rd
	s <- readIORef rs
	displayCursor <- readIORef rshape
	drawWorld w $ \w' -> displayCursor w' s d x y

getDirection :: Turtle -> IO Double
getDirection = readIORef . tDir

getPosition :: Turtle -> IO (Double, Double)
getPosition = readIORef . tPos

setPosition :: Turtle -> Double -> Double -> IO ()
setPosition t = curry $ writeIORef (tPos t)

setSize :: Turtle -> Double -> IO ()
setSize = writeIORef . tSize

setShape :: 
	Turtle -> (World -> Double -> Double -> Double -> Double -> IO ()) -> IO ()
setShape = writeIORef . tShape

initTurtle :: World -> IO Turtle
initTurtle w = do
	initPos <- newIORef $ error "pos is undefined"
	initDir <- newIORef $ error "dir is undefined"
	initSize <- newIORef $ error "size is undefined"
	initShape <- newIORef $ error "shape is undefined"
	ps <- newIORef PenDown

	let t = Turtle{
		tWorld = w,
		tPenState = ps,
		tPos = initPos,
		tDir = initDir,
		tSize = initSize,
		tShape = initShape
	 }

	addExposeAction w $ drawShape initShape initPos initDir initSize

	(width, height) <- winSize w
	writeIORef initPos (width / 2, height / 2)
	writeIORef initDir 0
	writeIORef initSize 2
	writeIORef initShape displayTurtle

	setPosition t 100 200
	setDirection t 0
	setSize t 2
	setShape t displayTurtle

	setPosition t (width / 2) (height / 2)
	drawTurtle t
	return t

shapesize :: Turtle -> Double -> IO ()
shapesize t s = do
	setSize t s
	drawTurtle t

position :: Turtle -> IO (Double, Double)
position t = do
	(x, y) <- getPosition t
	width <- windowWidth t
	height <- windowHeight t
	return (x - width / 2, height / 2 - y)

windowWidth :: Turtle -> IO Double
windowWidth = fmap fst . winSize . tWorld

windowHeight :: Turtle -> IO Double
windowHeight = fmap snd . winSize . tWorld

data PenState = PenUp | PenDown

penup :: Turtle -> IO ()
penup t = writeIORef (tPenState t) PenUp

pendown :: Turtle -> IO ()
pendown t = writeIORef (tPenState t) PenDown

isdown :: Turtle -> IO Bool
isdown t = do
	ps <- readIORef $ tPenState t
	return $ case ps of
		PenUp -> False
		PenDown -> True

data Buf = BG | UndoBuf

moveTurtle :: Turtle -> Double -> Double -> IO ()
moveTurtle t x2 y2 = do
	(x1, y1) <- getPosition t
	setPosition t x2 y2
	drawLine t x1 y1 x2 y2 BG
	drawTurtle t
	threadDelay 20000

drawLine :: Turtle -> Double -> Double -> Double -> Double -> Buf -> IO ()
drawLine t x1 y1 x2 y2 buf = do
		let w = tWorld t
		pd <- isdown t
		when pd $
			case buf of
				BG -> lineBG w x1 y1 x2 y2
				UndoBuf -> lineUndoBuf w x1 y1 x2 y2

rotateBy :: Turtle -> Double -> IO Double
rotateBy t dd = do
	d0 <- getDirection t
	let	nd = (d0 + dd) `gMod` 360
	setDirection t nd
	drawTurtle t
	return nd

gMod :: (Num a, Ord a) => a -> a -> a
x `gMod` y
	| x >= y = (x - y) `gMod` y
	| otherwise = x

clear :: Turtle -> IO (IO (), ((Double, Double), Double, Buf -> IO ()))
clear t = do
	let	w = tWorld t
		retAct = do
			clearBG w
			drawTurtle t
	pos <- getPosition t
	dir <- getDirection t
	let	pastAct buf = case buf of
			BG -> clearBG w
			UndoBuf -> clearUndoBuf w
	return (retAct, (pos, dir, pastAct))

displayTurtle :: World -> Double -> Double -> Double -> Double -> IO ()
displayTurtle w s d x y = drawCursor w $ getTurtle s d x y
--	$ map (uncurry (addDoubles (x, y)) . rotatePointD d . mulPoint s) turtle

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

initUndo :: Turtle -> IO ()
initUndo t = do
	let w = tWorld t
	clearBG w
	undoBufToBG w

flushW :: Turtle -> IO ()
flushW = drawTurtle

setDirection :: Turtle -> Double -> IO ()
setDirection t = writeIORef (tDir t)
