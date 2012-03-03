module Main where

import Graphics.X11.Turtle
import System.Random
import Control.Monad
import Control.Concurrent

main :: IO ()
main = initTurtle >>= forever . hitWall -- >> threadDelay 1000000

initTurtle :: IO Turtle
initTurtle = do
	f <- openField
	threadDelay 1000000
	t <- newTurtle f
	bgcolor f "gray" -- (128, 128, 128)
	shape t "turtle"
	shapesize t 2 2
	pensize t 10
	pencolor t "white"
	radians t
	randomRIO (- pi, pi) >>= setheading t
	onclick f $ \_ x y -> goto t x y >> return True
	return t

turnTop, turnBottom, turnLeft, turnRight :: Turtle -> IO ()
turnTop t = randomRIO (pi, 2 * pi) >>= setheading t
turnBottom t = randomRIO (0, pi) >>= setheading t
turnLeft t = randomRIO (- pi / 2, pi / 2) >>= setheading t
turnRight t = randomRIO (pi / 2, pi * 3 / 2) >>= setheading t

type Point = (Double, Double)

data Wall = TopWall | BottomWall | LeftWall | RightWall deriving Show

hitWall :: Turtle -> IO ()
hitWall t = do
	w <- getWall t
	case w of
		TopWall -> getTop t >>= toY t >> turnTop t
		BottomWall -> getBottom t >>= toY t >> turnBottom t
		LeftWall -> getLeft t >>= toX t >> turnLeft t
		RightWall -> getRight t >>= toX t >> turnRight t
	threadDelay 10000

getWall :: Turtle -> IO Wall
getWall t = do
	(rt, lt, lb, rb) <- getFourPoints t
	trt <- uncurry (towards t) rt
	tlt <- uncurry (towards t) lt
	tlb <- uncurry (towards t) lb
	trb <- uncurry (towards t) rb
	dir <- heading t
	return $ case dir of
		_	| trt <= dir && dir < tlt	-> TopWall
			| tlt <= dir && dir < tlb	-> LeftWall
			| tlb <= dir && dir < trb	-> BottomWall
			| otherwise			-> RightWall

getFourPoints :: Turtle -> IO (Point, Point, Point, Point)
getFourPoints t = do
	width <- windowWidth t
	height <- windowHeight t
	let	left = - width / 2
		right = width / 2
		top = height / 2
		bottom = - height / 2
	return ((right, top), (left, top), (left, bottom), (right, bottom))

getTop, getBottom, getLeft, getRight :: Turtle -> IO Double
getTop = fmap (/ 2) . windowHeight
getBottom = fmap (negate . (/ 2)) . windowHeight
getLeft = fmap (negate . (/ 2)) . windowWidth
getRight = fmap (/ 2) . windowWidth


toX, toY :: Turtle -> Double -> IO ()
toX t x = do
	dir <- heading t
	(x0, _) <- position t
	forward t $ (x - x0) / cos dir
toY t y = do
	dir <- heading t
	(_, y0) <- position t
	forward t $ (y - y0) / sin dir

nikoniko :: Turtle -> IO ()
nikoniko t = do
	penup t
	forward t 100
	left t 90
	pendown t
	circle t 100
	penup t
	left t 45
	forward t 80
	pendown t
	right t 45
	circle t 15
	left t 45
	penup t
	backward t 80
	right t 135
	backward t 200
	left t 45
	forward t 80
	pendown t
	right t 135
	circle t 15
	penup t
	right t 45
	forward t 80
	left t 135
	forward t 103
	left t 90
	pendown t
	circle t 3
	right t 90
	penup t
	forward t 9
	right t 90
	forward t 70
	pendown t
	right t 60
	forward t 15
	right t 60
	forward t 15
	hideturtle t
