module Main where

import Graphics.X11.Turtle

main :: IO ()
main = do
	f <- openField
	t <- newTurtle f
	shape t "turtle"
	shapesize t 2 2
	pensize t 10
	pencolor t (0, 127, 0)
	onclick f $ \_ x y -> goto t x y >> return True
	waitField f
	putStrLn "end"
