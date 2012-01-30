module Graphics.X11.TurtleInput (
	TurtleState,
	TurtleInput(..),

	getTurtleStates,
	getPosition,
	penState,
	undoNum
) where

import Graphics.X11.TurtleState(TurtleState(..), initialTurtleState)
import Control.Concurrent.Chan(Chan, newChan, getChanContents)
import Prelude hiding(Left)

getPosition :: TurtleState -> (Double, Double)
getPosition = position

undoNum :: TurtleState -> Int
undoNum = undonum

penState :: TurtleState -> Bool
penState = pendown

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

getTurtleStates :: [(Double, Double)] -> IO (Chan TurtleInput, [TurtleState])
getTurtleStates sh = do
	let	ts0 = initialTurtleState sh
	(c, tis) <- makeInput
	return (c, ts0 : ts0 : inputToTurtle [] ts0 tis)

makeInput :: IO (Chan TurtleInput, [TurtleInput])
makeInput = do
	c <- newChan
	tis <- getChanContents c
	return (c, tis)

clearState :: TurtleState -> TurtleState
clearState t = t{
	line = False,
	undo = False,
	undonum = 1
 }

nextTurtle :: TurtleState -> TurtleInput -> TurtleState
nextTurtle ts0 (Shape sh) = (clearState ts0){shape = sh}
nextTurtle ts0 (ShapeSize ss) = (clearState ts0){size = ss}
nextTurtle ts0 (Goto x y) =
	(clearState ts0){position = (x, y), line = pendown ts0}
nextTurtle ts0 (RotateTo d) = (clearState ts0){direction = d}
nextTurtle ts0 PenDown = (clearState ts0){pendown = True}
nextTurtle ts0 PenUp = (clearState ts0){pendown = False}
nextTurtle ts0 (SetUndoNum un) = (clearState ts0){undonum = un}
nextTurtle _ _ = error "not defined"

inputToTurtle :: [TurtleState] -> TurtleState -> [TurtleInput] -> [TurtleState]
inputToTurtle (tsb : tsbs) _ (Undo : ti) =
	let nts = tsb{undo = True} in nts : inputToTurtle tsbs nts ti
inputToTurtle tsbs ts0 (Forward len : ti) = let
	(x0, y0) = position ts0
	dir = direction ts0
	x = x0 + len * cos (dir * pi / 180)
	y = y0 + len * sin (dir * pi / 180) in
	inputToTurtle tsbs ts0 (Goto x y : ti)
inputToTurtle tsbs ts0 (Left dd : ti) =
	inputToTurtle tsbs ts0 $ RotateTo (direction ts0 + dd) : ti
inputToTurtle tsbs ts0 (ti : tis) =
	let nts = nextTurtle ts0 ti in nts : inputToTurtle (ts0 : tsbs) nts tis
inputToTurtle _ _ [] = error "no more input"
