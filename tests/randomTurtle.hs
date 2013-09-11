module Main where

import Graphics.X11.Turtle
import System.Random
import Control.Monad
import Control.Concurrent
import Data.Word

import System.Environment

main :: IO ()
main = do
	args <- getArgs
	let time = case args of
		[] -> 10000000
		a : _ -> read a * 1000000
	putStrLn "testNewTurtle"
	f <- openField
	t <- newTurtle f
	forkIO $ randomTurtle t
	threadDelay time
	closeField f

randomWord8 :: IO Word8
randomWord8 = fmap fromIntegral $ (randomRIO (0, 255) :: IO Int)

randomTurtle :: Turtle -> IO ()
randomTurtle t = do
	penup t
	shape t "turtle"
	shapesize t 2 2
	(x0, y0) <- position t
	forward t 100
	pendown t
	left t 90
	circle t 100
	penup t
	goto t x0 y0
--	home t
	pendown t
	position t >>= print
	sequence_ $ repeat $ do
		d <- randomRIO (- 180, 180)
		r <- randomWord8
		g <- randomWord8
		b <- randomWord8
		pencolor t (r, g, b)
		left t d
		forward t 15
		d <- distance t x0 y0
		when (d > 100) $ undo t
		threadDelay 10000
