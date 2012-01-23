module TurtleInput (
	TurtleState,

	makeInput,
	inputToTurtle,
	TurtleInput(..),
	initialTurtleState,
	classic
) where

import Control.Concurrent
import System.IO.Unsafe
import TurtleState

main :: IO ()
main = do
	putStrLn "module TurtleInput"
	(c, ret) <- makeInput
	writeChan c $ Shape classic
	writeChan c $ ShapeSize 1
	writeChan c PenDown
	writeChan c $ Goto 0 0
	writeChan c $ RotateTo 0
	writeChan c $ RotateTo 180
	writeChan c Undo
	print $ take 6 ret
	print $ drop 5 $ take 6 $ inputToTurtle [] initialTurtleState ret

data TurtleInput
	= Shape [(Double, Double)]
	| ShapeSize Double
	| Goto Double Double
	| RotateTo Double
	| PenUp
	| PenDown
	| Undo
	deriving Show

makeInput :: IO (Chan TurtleInput, [TurtleInput])
makeInput = do
	c <- newChan
	ret <- getInput c
	return (c, ret)

getInput :: Chan TurtleInput -> IO [TurtleInput]
getInput c = unsafeInterleaveIO $ do
	ti <- readChan c
	tis <- getInput c
	return $ ti : tis

inputToTurtle :: [TurtleState] -> TurtleState -> [TurtleInput] -> [TurtleState]
inputToTurtle tsbs ts0 (Shape sh : ti) =
	ts0{turtleShape = sh} : inputToTurtle (ts0 : tsbs) ts0{turtleShape = sh} ti
inputToTurtle tsbs ts0 (ShapeSize ss : ti) =
	ts0{turtleSize = ss} : inputToTurtle (ts0 : tsbs) ts0{turtleSize = ss} ti
inputToTurtle tsbs ts0 (Goto x y : ti) =
	ts0{turtlePos = (x, y)} : inputToTurtle (ts0 : tsbs) ts0{turtlePos = (x, y)} ti
inputToTurtle tsbs ts0 (RotateTo d : ti) =
	ts0{turtleDir = d} : inputToTurtle (ts0 : tsbs) ts0{turtleDir = d} ti
inputToTurtle tsbs ts0 (PenDown : ti) =
	ts0{turtlePenDown = True} : inputToTurtle (ts0 : tsbs) ts0{turtlePenDown = True} ti
inputToTurtle (tsb : tsbs) ts0 (Undo : ti) =
	tsb{turtleUndo = True} : inputToTurtle tsbs tsb{turtleUndo = True} ti
