module Main where

import Graphics.X11.Turtle
import Text.XML.YJSVG
import System.Random
import Control.Monad
import Control.Concurrent
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	case args of
		[] -> do
			w <- newChan
			(f, t1, t2) <- twoFlowers
			onkeypress f $ \c -> do
				writeChan w ()
				return $ c /= 'q'
			readChan w
			replicateM_ 500 $ undo t1 >> undo t2
			waitField f
		["1"] -> do
			f <- openField
			t <- newTurtle f
			bgcolor t "blue"
			flower t 5
			threadDelay 1000000
			w <- windowWidth t
			h <- windowHeight t
			getSVG t >>= putStrLn . showSVG w h
--			replicateM_ 500 $ undo t
			waitField f

qcircle :: Turtle -> Double -> IO ()
qcircle t s = replicateM_ 9 $ forward t s >> right t 10

leaf :: Turtle -> Double -> IO ()
leaf t s = beginfill t >> qcircle t s >> right t 90 >> qcircle t s >> endfill t

twoFlowers :: IO (Field, Turtle, Turtle)
twoFlowers = do
	wait <- newChan
	f <- openField
	onkeypress f $ return . (/= 'q')
	t1 <- newTurtle f
	t2 <- newTurtle f
	bgcolor t1 "skyblue"
	bgcolor t2 "blue"
	goto t1 (- 150) 0
	goto t2 100 (- 80)
	shape t1 "turtle"
	shapesize t1 2 2
	shape t2 "turtle"
	forkIO $ flower t1 10 >> writeChan wait ()
	forkIO $ flower t2 5 >> writeChan wait ()
	readChan wait
	readChan wait
	hideturtle t1
	hideturtle t2
	return (f, t1, t2)

flower :: Turtle -> Double -> IO ()
flower t s = do
	pencolor t "red"
	left t 90
	forward t $ 5 * s
	clear t
	replicateM_ 9 $ leaf t s >> right t 10
	right t 180
	penup t
	forward t $ 6 * s
	pendown t
	pencolor t "green"
	pensize t s
	forward t $ 14 * s
	right t 180
	forward t $ 3 * s
	right t 20
	leaf t s

randomTurtle :: IO ()
randomTurtle = do
	f <- openField
	threadDelay 1000000
	t <- newTurtle f
	shape t "turtle"
	shapesize t 3 3
	penup t
	forward t 150
	pendown t
	left t 90
	circle t 150
	penup t
	home t
	pendown t
	sequence_ $ repeat $ do
		randomRIO (-180, 180) >>= left t >> forward t 15
		d <- distance t 0 0
		when (d > 150) $ undo t

testClear :: IO (Turtle, Turtle, Turtle, Turtle)
testClear = do
	f <- openField
	threadDelay 1000000
	t1 <- newTurtle f
	t2 <- newTurtle f
	t3 <- newTurtle f
	t4 <- newTurtle f
	forward t1 150
	left t1 90
	circle t1 150
	right t2 90
	forward t2 170
	right t2 90
	forward t2 200
	left t2 180
	circle t2 50
	circle t3 30
	left t4 180
	circle t4 50
	return (t1, t2, t3, t4)

initForTest :: IO Turtle
initForTest = do
	f <- openField
	threadDelay 1000000
	t <- newTurtle f
	shape t "turtle"
	shapesize t 3 3
	forward t 150
	forward t 500
	backward t 500
	right t 90
	left t 180
	circle t 150
	undo t
	return t

testModuleCharAndBG :: IO ()
testModuleCharAndBG = do
	w <- openField
	threadDelay 1000000
	s <- newTurtle w
	s1 <- newTurtle w
	shape s1 "turtle"
	shapesize s1 1 1
	goto s 50 105
	goto s1 100 30
	goto s 25 100
	goto s1 10 30
	goto s 150 100
	shapesize s1 2 2
	undo s
	goto s 150 150
	left s1 90
	undo s
	goto s 150 50
	undo s
	undo s1
	undo s
