module Main where

import Graphics.X11.Turtle

main :: IO ()
main = do
	f <- openField
	t <- newTurtle f
	shape t "turtle"
	shapesize t 2
	pensize t 10
	pencolor t 255 0 0
	left t 150
	penup t
	forward t 200
	right t 150
	pendown t
	forward t 50
	penup t
	right t 150
	forward t $ 50 * 2 / 3 ** (1 / 2)
	left t 150
	pendown t
	forward t 50
	hideturtle t
	waitField f
