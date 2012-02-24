module Main where

import Graphics.X11.Turtle

centerCircle :: Turtle -> Double -> IO ()
centerCircle t r = do
	penup t
	forward t r
	left t 90
	pendown t
	circle t r
	left t 90
	penup t
	forward t r
	pendown t
