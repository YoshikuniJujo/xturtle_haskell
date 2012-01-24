module Graphics.X11.TurtleInput (
	TurtleState(turtlePos, turtleUndoNum, turtlePenDown),

	makeInput,
	inputToTurtle,
	TurtleInput(..),
	initialTurtleState
) where

import Control.Concurrent
import System.IO.Unsafe
import Graphics.X11.TurtleState
import Prelude hiding (Left)

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
	| SetUndoNum Int
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

nextTurtle :: TurtleState -> TurtleState
nextTurtle t = TurtleState{
	turtleShape = turtleShape t,
	turtleSize = turtleSize t,
	turtlePos = turtlePos t,
	turtleDir = turtleDir t,
	turtlePenDown = turtlePenDown t,
	turtleUndo = False,
	turtleUndoNum = 1
 }

inputToTurtleNext :: [TurtleState] -> TurtleState -> [TurtleInput] -> [TurtleState]
inputToTurtleNext tsbs nts ti = nts : inputToTurtle tsbs nts ti

inputToTurtle :: [TurtleState] -> TurtleState -> [TurtleInput] -> [TurtleState]
inputToTurtle (tsb : tsbs) _ (Undo : ti) =
	tsb{turtleUndo = True } : inputToTurtle tsbs tsb{turtleUndo = True} ti
inputToTurtle tsbs ts0 (Shape sh : ti) =
	inputToTurtleNext (ts0 : tsbs) (nextTurtle ts0){turtleShape = sh} ti
inputToTurtle tsbs ts0 (ShapeSize ss : ti) =
	inputToTurtleNext (ts0 : tsbs) (nextTurtle ts0){turtleSize = ss} ti
inputToTurtle tsbs ts0 (Goto x y : ti) =
	inputToTurtleNext (ts0 : tsbs) (nextTurtle ts0){turtlePos = (x, y)} ti
inputToTurtle tsbs ts0 (RotateTo d : ti) =
	inputToTurtleNext (ts0 : tsbs) (nextTurtle ts0){turtleDir = d} ti
inputToTurtle tsbs ts0 (PenDown : ti) =
	inputToTurtleNext (ts0 : tsbs) (nextTurtle ts0){turtlePenDown = True} ti
inputToTurtle tsbs ts0 (PenUp : ti) =
	inputToTurtleNext (ts0 : tsbs) (nextTurtle ts0){turtlePenDown = False} ti
inputToTurtle tsbs ts0 (SetUndoNum un : ti) =
	inputToTurtleNext (ts0 : tsbs) (nextTurtle ts0){turtleUndoNum = un} ti
inputToTurtle tsbs ts0 (Forward len : ti) = let
	(x0, y0) = turtlePos ts0
	dir = turtleDir ts0
	x = x0 + len * cos (dir * pi / 180)
	y = y0 + len * sin (dir * pi / 180) in
	inputToTurtle tsbs ts0 (Goto x y : ti)
inputToTurtle tsbs ts0 (Left dd : ti) =
	inputToTurtle tsbs ts0 $ RotateTo (turtleDir ts0 + dd) : ti
inputToTurtle _ _ _ = error "bad condition in inputToTurtle"
