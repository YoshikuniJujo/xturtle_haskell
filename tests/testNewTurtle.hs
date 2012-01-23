module Main where

import NewTurtle
import System.Random
import Control.Monad
import Control.Concurrent

randomTurtle :: Turtle -> IO ()
randomTurtle t = do
	forward t 100
	left t 90
	circle t 100
--	home t
	position t >>= print
	sequence_ $ repeat $ do
		d <- randomRIO (- 180, 180)
		left t d
		forward t 15
		d <- distance t 0 0
		when (d > 100) $ undo t
		threadDelay 50000
