module Main where

import Graphics.X11.Turtle
import Data.IORef
import Control.Concurrent
import Control.Monad
import System.Environment
import Text.XML.YJSVG
import Data.Word

main :: IO ()
main = do
	[fn, save] <- getArgs
	clr <- newIORef 0
	bgclr <- newIORef 0
	f <- openField
	t <- newTurtle f
	threadDelay 100000
	height <- windowHeight t
	width <- windowWidth t
	clrT <- newTurtle f
	pensize clrT 10
	hideturtle clrT
	penup clrT
	goto clrT (width / 2 - 10) (height / 2 - 10)
	pendown clrT
	shape t "turtle"
	shapesize t 2 2
	pensize t 10
	hideturtle t
	penup t
	readFile fn >>= runInputs t . read
	onclick f $ \b x y -> do
		case b of
			1 -> goto t x y >> pendown t >> forward t 0 >> return True
			3 -> clear t >> return True
			2 -> modifyIORef bgclr (+ 1) >> readIORef bgclr >>=
				(\c -> bgcolor t c) . (colors !!) >> return True
			4 -> goto t x y >> modifyIORef clr (+ 6) >> readIORef clr >>=
				(\c -> pencolor t c >> pencolor clrT c) . (colors !!) >>
				forward clrT 0 >> return True
			5 -> goto t x y >> modifyIORef clr (+ 1) >> readIORef clr >>=
				(\c -> pencolor t c >> pencolor clrT c) . (colors !!) >>
				forward clrT 0 >> return True
	onrelease f $ \_ _ _ -> penup t>> return True
	onkeypress f $ \_ -> do
{-
		print "hello"
		print $ head inputs
		print inputs
		print $ length inputs
-}
		w <- windowWidth t
		h <- windowHeight t
		getSVG t >>= putStrLn . showSVG w h
		inputs <- inputs t
		when (read save) $ writeFile fn $ show $ drop 5 inputs
		return False
	ondrag f $ \bn x y -> case bn of
		1 -> goto t x y
		_ -> return ()
	waitField f

colors :: [(Word8, Word8, Word8)]
colors = cycle [
	(255, 0, 0), (0, 255, 0), (255, 255, 0), (0, 0, 255),
	(128, 76, 0), (255, 255, 255), (0, 0, 0)]
