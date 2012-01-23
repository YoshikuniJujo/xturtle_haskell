module Graphics.X11.TurtleState (
	TurtleState(..),
	initialTurtleState,
	classic,
	turtle
) where

import Control.Arrow

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

classic :: [(Double, Double)]
classic = clssc ++ reverse (map (second negate) clssc)
	where
	clssc = [
		(- 10, 0),
		(- 16, 6),
		(0, 0)
	 ]

turtle :: [(Double, Double)]
turtle = ttl ++ reverse (map (second negate) ttl)
	where
	ttl = [
		(- 10, 0),
		(- 8, - 3),
		(- 10, - 5),
		(- 7, - 9),
		(- 5, - 6),
		(0, - 8),
		(4, - 7),
		(6, - 10),
		(8, - 7),
		(7, - 5),
		(10, - 2),
		(13, - 3),
		(16, 0)
	 ]
