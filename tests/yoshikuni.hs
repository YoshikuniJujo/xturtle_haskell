module Main where

import Graphics.X11.Turtle
import Control.Monad

main :: IO ()
main = do
	print xturtleVersion
	f <- openField
	t <- newTurtle f
	pencolor t (0 :: Int, 128 :: Int, 0 :: Int)
	pensize t 10
	shape t "turtle"
	shapesize t 2 2
	penup t
	left t 130
	forward t 170
	pendown t
	left t 50
	forward t 40
	left t 90
	forward t 100
	replicateM_ 27 $ right t 10 >> forward t 5
	forward t 40
	replicateM_ 5 $ right t 10 >> forward t 5
	penup t
	left t 120
	forward t 90
	right t 160
	pendown t
	forward t 110
	replicateM_ 20 $ left t 10 >> forward t 6
	penup t
	right t 50
	forward t 110
	pendown t
	backward t 80
	right t 100
	forward t 70
	penup t
	left t 120
	forward t 120
	pendown t
	right t 170
	forward t 140
	left t 120
	forward t 20
	penup t
	left t 50
	forward t 120
	pendown t
	right t 70
	forward t 10
	right t 10
	forward t 30
	heading t >>= print
	right t 110
	penup t
	forward t 100
	pendown t
	left t 100
	forward t 10
	left t 10
	forward t 30
	right t 120
	hideturtle t
	penup t
	forward t 100
	write t "" 10 "xturtle"
	right t 40
	forward t 30
	write t "" 10 "by Yoshikuni Jujo"
	onclick f $ \_ x y -> do
		goto t x y
		pendown t
		forward t 0
		penup t
		return True
	onrelease f $ \_ _ _ -> do
		replicateM_ 4 $ undo t
		return True
	t2 <- newTurtle f
	hideturtle t2
	penup t2
	goto t2 (- 200) (- 100)
	onkeypress f $ \c -> do
		write t2 "" 10 [c]
		forward t2 10
		return $ c /= 'q'
	waitField f
