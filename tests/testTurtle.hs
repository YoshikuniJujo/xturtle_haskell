module Main where

import Graphics.X11.Turtle
import System.Random
import Control.Monad
import Control.Concurrent

randomTurtle :: IO ()
randomTurtle = do
	f <- openField
	threadDelay 1000000
	t <- newTurtle f
	shape t "turtle"
	shapesize t 3
	penup t
	forward t 150
	pendown t
	left t 90
	circle t 150
	penup t
	home t
	pendown t
	sequence_ $ repeat $ do
		randomRIO (-180, 180) >>= left t >> forward t 15
		d <- distance t 0 0
		when (d > 150) $ undo t

testClear :: IO (Turtle, Turtle, Turtle, Turtle)
testClear = do
	f <- openField
	threadDelay 1000000
	t1 <- newTurtle f
	t2 <- newTurtle f
	t3 <- newTurtle f
	t4 <- newTurtle f
	forward t1 150
	left t1 90
	circle t1 150
	right t2 90
	forward t2 170
	right t2 90
	forward t2 200
	left t2 180
	circle t2 50
	circle t3 30
	left t4 180
	circle t4 50
	return (t1, t2, t3, t4)

initForTest :: IO Turtle
initForTest = do
	f <- openField
	threadDelay 1000000
	t <- newTurtle f
	shape t "turtle"
	shapesize t 3
	forward t 150
	forward t 500
	backward t 500
	right t 90
	left t 180
	circle t 150
	undo t
	return t
