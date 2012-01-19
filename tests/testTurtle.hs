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
