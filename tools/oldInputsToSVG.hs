module Main where

import TurtleInput
import Control.Concurrent
import Text.XML.YJSVG
import System.Environment

width, height :: Double
width = 1000
height = 700

main :: IO ()
main = do
	[fn] <- getArgs
	readFile fn >>= inputsToTurtle . read >>= putStrLn . turtleToSVG width height

inputsToTurtle :: [TurtleInput] -> IO TurtleState
inputsToTurtle is = do
	(ic, tis, sts) <- getTurtleStates []
	mapM_ (writeChan ic) is'
	return $ sts !! (length is' + 1)
	where
	is' = [Pensize 10, Penup] ++ is

turtleToSVG :: Double -> Double -> TurtleState -> String
turtleToSVG w h ts = showSVG w h $ reverse $ drawed ts ++
	[Rect (TopLeft 0 0) width height 0 (RGB 0x80 0x4c 0x00) (RGB 0 0 0)]
