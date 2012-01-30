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
	turtleUndoNum :: Int
 } deriving Show

initialTurtleState :: [(Double, Double)] -> TurtleState
initialTurtleState sh = TurtleState {
	turtleShape = sh,
	turtleSize = 1,
	turtlePos = (0, 0),
	turtleDir = 0,
	turtlePenDown = False,
	turtleUndo = False,
	turtleUndoNum = 1
 }
