module Main where

import Graphics.X11.Turtle
import System.Environment
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
	n <- fmap (read . head) getArgs
	f <- openField
	t <- newTurtle f
	penup t
	threadDelay 1000000
	h <- windowHeight t
	w <- windowWidth t
	hideturtle t
	goto t (- w / 2) (h / 2)
	mapM_ (turtleWrite t h) $ fizzBuzz n
	replicateM_ (n * 5) $ undo t
	waitField f

turtleWrite :: Turtle -> Double -> (Double, Double, Double, String) -> IO ()
turtleWrite t h (r, g, b, s) = do
	pencolor t $ rgbToColor r g b
	setheading t $ - 90
	forward t 10
	(_, y) <- position t
	when (y < - h / 2) $ do
		sety t $ h / 2 - 10
		setheading t 0
		forward t 50
	write t "" 8 s

fizzBuzz :: Int -> [(Double, Double, Double, String)]
fizzBuzz n = map fizzBuzzOne [1 .. n] -- map ((,,,) 1 0 0) fb
{-
	where
	fb = zipWith (\n s -> if null s then show n else s) [1 .. n] $
		zipWith (++) (cycle ["", "", "Fizz"]) (cycle ["", "", "", "", "Buzz"])
-}

fizzBuzzOne :: Int -> (Double, Double, Double, String)
fizzBuzzOne n
	| n `mod` 5 /= 0 && n `mod` 3 /= 0 = (0, 0, 0, show n)
	| n `mod` 5 /= 0 = (1, 0, 0, "Fizz")
	| n `mod` 3 /= 0 = (0, 0, 1, "Buzz")
	| otherwise = (1, 0, 1, "FizzBuzz")
