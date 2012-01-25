module Main where

import Graphics.X11.Turtle
import System.Random
import Control.Monad
import Control.Concurrent

main :: IO ()
main = do
	putStrLn "testNewTurtle"

randomTurtle :: Turtle -> IO ()
randomTurtle t = do
	shape t "turtle"
	shapesize t 2
	penup t
	forward t 100
	pendown t
	left t 90
	circle t 100
	penup t
	home t
	pendown t
	position t >>= print
	sequence_ $ repeat $ do
		d <- randomRIO (- 180, 180)
		left t d
		forward t 15
		d <- distance t 0 0
		when (d > 100) $ undo t
--		threadDelay 50000
