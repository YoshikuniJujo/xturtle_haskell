module Main where

import Graphics.X11.Turtle
import Data.IORef
import Control.Concurrent
import Control.Monad
import System.Environment
import Text.XML.YJSVG

main :: IO ()
main = do
	[fn, save] <- getArgs
	clr <- newIORef 0
	bgclr <- newIORef 0
	f <- openField
	t <- newTurtle f
	threadDelay 1000000
	height <- windowHeight t
	width <- windowWidth t
	clrT <- newTurtle f
	pensize clrT 10
	hideturtle clrT
	penup clrT
	goto clrT (width / 2 - 10) (height / 2 - 10)
	pendown clrT
	shape t "turtle"
	shapesize t 2
	pensize t 10
	hideturtle t
	penup t
	readFile fn >>= sendInputs t . read
	onclick f $ \b x y -> do
		case b of
			1 -> goto t x y >> pendown t >> forward t 0 >> return True
			3 -> clear t >> return True
			2 -> modifyIORef bgclr (+ 1) >> readIORef bgclr >>=
				(\(r, g, b) -> bgcolor f r g b) . (colors !!) >> return True
			4 -> goto t x y >> modifyIORef clr (+ 6) >> readIORef clr >>=
				(\(r, g, b) -> pencolor t r g b >> pencolor clrT r g b) . (colors !!) >>
				forward clrT 0 >> return True
			5 -> goto t x y >> modifyIORef clr (+ 1) >> readIORef clr >>=
				(\(r, g, b) -> pencolor t r g b >> pencolor clrT r g b) . (colors !!) >>
				forward clrT 0 >> return True
	onrelease f $ \_ _ -> penup t>> return True
	onkeypress f $ \_ -> do
{-
		print "hello"
		print $ head inputs
		print inputs
		print $ length inputs
-}
		getSVG t >>= putStrLn . showSVG 1000 1000
		inputs <- getInputs t
		when (read save) $ writeFile fn $ show $ drop 5 inputs
		return False
	ondrag f $ \x y -> goto t x y
	waitField f

colors :: [(Double, Double, Double)]
colors = cycle [(1, 0, 0), (0, 1, 0), (1, 1, 0), (0, 0, 1), (0.5, 0.3, 0), (1, 1, 1), (0, 0, 0)]
