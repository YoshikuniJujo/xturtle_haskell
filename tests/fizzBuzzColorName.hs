module Main where

import Graphics.X11.Turtle
import System.Environment
import Control.Concurrent
import Control.Monad
import Data.Word

type Color = String

main :: IO ()
main = do
	n <- fmap (read . head) getArgs
	f <- openField
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	penup t
	speed t "slowest"
	threadDelay 1000000
	h <- windowHeight t
	w <- windowWidth t
	hideturtle t
	goto t (- w / 2) (h / 2)
	mapM_ (turtleWrite t h) $ fizzBuzz n
	threadDelay 5000000
	replicateM_ (n * 5) $ undo t
	waitField f

turtleWrite :: Turtle -> Double -> (Color, String) -> IO ()
turtleWrite t h (c, s) = do
	pencolor t c
	setheading t $ - 90
	forward t 10
	(_, y) <- position t
	when (y < - h / 2) $ do
		sety t $ h / 2 - 10
		setheading t 0
		forward t 50
	write t "" 8 s

fizzBuzz :: Int -> [(Color, String)]
fizzBuzz n = map fizzBuzzOne [1 .. n]

fizzBuzzOne :: Int -> (Color, String)
fizzBuzzOne n
	| n `mod` 5 /= 0 && n `mod` 3 /= 0 = ("black", show n)
	| n `mod` 5 /= 0 = ("red", "Fizz")
	| n `mod` 3 /= 0 = ("blue", "Buzz")
	| otherwise = ("violet", "FizzBuzz")
