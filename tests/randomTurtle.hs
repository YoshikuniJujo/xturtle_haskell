module Main where

import Graphics.X11.Turtle
import System.Random
import Control.Monad
import Control.Concurrent
import Data.Word

main :: IO ()
main = do
	putStrLn "testNewTurtle"

randomWord8 :: IO Word8
randomWord8 = fmap fromIntegral $ (randomRIO (0, 255) :: IO Int)

randomTurtle :: Turtle -> IO ()
randomTurtle t = do
	penup t
	shape t "turtle"
	shapesize t 2
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
		r <- randomRIO (0, 1)
		g <- randomRIO (0, 1)
		b <- randomRIO (0, 1)
		pencolor t r g b
		left t d
		forward t 15
		d <- distance t 0 0
		when (d > 100) $ undo t
--		threadDelay 50000
