module Main where

import Graphics.X11.Turtle

main :: IO ()
main = do
	f <- openField
	t <- newTurtle f
	penup t
	goto t (- 200) 180
	mapM_ (turtleWrite t) $ fizzBuzz 35
	waitField f

turtleWrite :: Turtle -> String -> IO ()
turtleWrite t s = do
	setheading t $ - 90
	forward t 10
	write t "" 8 s

fizzBuzz :: Int -> [String]
fizzBuzz n = zipWith (\n s -> if null s then show n else s) [1 .. n] $
	zipWith (++) (cycle ["", "", "Fizz"]) (cycle ["", "", "", "", "Buzz"])
