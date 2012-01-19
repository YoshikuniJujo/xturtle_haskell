module Graphics.X11.Turtle (
	Turtle,
	openField,
	newTurtle,
	shape,
	shapesize,
	forward,
	backward,
	circle,
	undo,
	left,
	right,
	clear,
	home,
	pendown,
	penup,
	isdown,
	distance
) where

import Graphics.X11.CharAndBG
import Control.Monad

forward, backward, forwardNotSetUndo :: Turtle -> Double -> IO ()
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
	dir <- direction t
	rotate t (dir + dd)

right t = left t . negate

circle :: Turtle -> Double -> IO ()
circle t r = do
	setUndoN t 73
	forwardNotSetUndo t (r * pi / 36)
	left t 10
	replicateM_ 35 $ forwardNotSetUndo t (2 * r * pi / 36) >>
		left t 10
	forwardNotSetUndo t (r * pi / 36)

home :: Turtle -> IO ()
home t = do
	goto t 0 0
	rotate t 0

distance :: Turtle -> Double -> Double -> IO Double
distance t x0 y0 = do
	(x, y) <- position t
	return $ ((x - x0) ** 2 + (y - y0) ** 2) ** (1 / 2)
