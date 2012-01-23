module Graphics.X11.TurtleInput (
	TurtleState(turtlePos),

	makeInput,
	inputToTurtle,
	TurtleInput(..),
	initialTurtleState,
	classic,
	turtle
) where

import Control.Concurrent
import System.IO.Unsafe
import Graphics.X11.TurtleState
import Prelude hiding (Left)

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
	| Forward Double
	| Left Double
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
inputToTurtle tsbs ts0 (Shape sh : ti) = let
	nts = ts0{turtleShape = sh, turtleLineDone = False} in
	nts : inputToTurtle (ts0 : tsbs) nts ti
inputToTurtle tsbs ts0 (ShapeSize ss : ti) = let
	nts = ts0{turtleSize = ss, turtleLineDone = False} in
	nts : inputToTurtle (ts0 : tsbs) nts ti
{-
	ts0{turtleSize = ss} :
		inputToTurtle (ts0 : tsbs) ts0{turtleSize = ss} ti
-}
inputToTurtle tsbs ts0 (Goto x y : ti) = let
--	tsbs' = if length tsbs > 10 then take 10 tsbs else tsbs in
	tsbs' = tsbs in
	ts0{turtlePos = (x, y), turtleLineDone = True, turtleUndo = False} :
		inputToTurtle (ts0 : tsbs') ts0{turtlePos = (x, y), turtleLineDone = True} ti
inputToTurtle tsbs ts0 (RotateTo d : ti) = let
	nts = ts0{turtleDir = d, turtleLineDone = False, turtleUndo = False} in
	nts : inputToTurtle (ts0 : tsbs) nts ti
--	ts0{turtleDir = d} : inputToTurtle (ts0 : tsbs) ts0{turtleDir = d} ti
inputToTurtle tsbs ts0 (PenDown : ti) =
	ts0{turtlePenDown = True} : inputToTurtle (ts0 : tsbs) ts0{turtlePenDown = True} ti
inputToTurtle (tsb : tsbs) ts0 (Undo : ti) =
	tsb{turtleUndo = True } :
		inputToTurtle tsbs tsb{turtleUndo = True} ti
inputToTurtle tsbs ts0 (Forward len : ti) = let
--	tsbs' = if length tsbs > 10 then take 10 tsbs else tsbs
	tsbs' = tsbs
	dir = turtleDir ts0
	(x0, y0) = turtlePos ts0
	x = x0 + len * cos (dir * pi / 180)
	y = y0 + len * sin (dir * pi / 180) in
	inputToTurtle tsbs' ts0 (Goto x y : ti)
inputToTurtle tsbs ts0 (Left dd : ti) = let
	dir = turtleDir ts0 + dd in
	inputToTurtle tsbs ts0 (RotateTo dir : ti)
