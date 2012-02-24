module Main where

import Graphics.X11.Turtle

test1 :: Turtle -> IO ()
test1 t = do
	forward t 100
	left t 90
	forward t 100
	left t 90
	forward t 100
	undo t
