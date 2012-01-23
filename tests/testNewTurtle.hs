module Main where

import NewTurtle
import System.Random
import Control.Monad

randomTurtle :: Turtle -> IO ()
randomTurtle t = do
	sequence_ $ repeat $ do
		d <- randomRIO (- 180, 180)
		left t d
		forward t 15
	
