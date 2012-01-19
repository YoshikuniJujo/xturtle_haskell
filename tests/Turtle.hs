module Turtle (
	openField,
	newTurtle,
	shape,
	shapesize,
) where

import CharAndBG
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
	putStrLn "module Turtle"

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

forward, backward :: Turtle -> Double -> IO ()
forward t dist = do
	setUndoN t 1
	forwardNotSetUndo t dist

forwardNotSetUndo t dist = do
	dir <- direction t
	(x0, y0) <- position t
	let	xd = dist * cos (dir * pi / 180)
		yd = dist * sin (dir * pi / 180)
	goto t (x0 + xd) (y0 + yd)


backward t = forward t . negate

left, right :: Turtle -> Double -> IO ()
left t dd = do
--	setUndoN t 0
	leftNotSetUndo t dd
leftNotSetUndo t dd = do
	dir <- direction t
	rotate t (dir + dd)

right t = left t . negate

circle :: Turtle -> Double -> IO ()
circle t r = do
	setUndoN t 72
	replicateM_ 36 $ forwardNotSetUndo t (2 * r * pi / 36) >>
		leftNotSetUndo t 10

home :: Turtle -> IO ()
home t = do
	goto t 0 0
	rotate t 0

distance :: Turtle -> Double -> Double -> IO Double
distance t x0 y0 = do
	(x, y) <- position t
	return $ ((x - x0) ^ 2 + (y - y0) ^ 2) ** (1 / 2)
