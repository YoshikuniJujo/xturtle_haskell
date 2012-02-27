module Main where

import Graphics.X11.Turtle
import Control.Monad

main :: IO ()
main = do
	print xturtleVersion
	f <- openField
	t <- newTurtle f
	shape t "turtle"
	replicateM_ 4 $ do
		replicateM_ 36 $ forward t 10 >> left t 10
		right t 90
	shapesize t 2
	pensize t 10
	replicateM_ 4 $ do
		replicateM_ 36 $ forward t 10 >> left t 10
		right t 90
	replicateM_ 600 $ undo t
	waitField f
	putStrLn "end"
