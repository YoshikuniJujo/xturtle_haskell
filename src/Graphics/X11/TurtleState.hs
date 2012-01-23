module Graphics.X11.TurtleState (
	TurtleState(..),
	initialTurtleState,
) where

data TurtleState = TurtleState {
	turtleShape :: [(Double, Double)],
	turtleSize :: Double,
	turtlePos :: (Double, Double),
	turtleDir :: Double,
	turtlePenDown :: Bool,
	turtleUndo :: Bool,
	turtleLineDone :: Bool,
	turtleUndoNum :: Int
 } deriving Show

initialTurtleState :: TurtleState
initialTurtleState = TurtleState {
	turtleShape = isUndefined "turtleShape",
	turtleSize = isUndefined "turtleSize",
	turtlePos = isUndefined "turtlePos",
	turtleDir = isUndefined "turtleDir",
	turtlePenDown = isUndefined "turtlePenDown",
	turtleUndo = False,
	turtleLineDone = False,
	turtleUndoNum = 1
 }

isUndefined :: String -> a
isUndefined name = error $ name ++ " is undefined"
