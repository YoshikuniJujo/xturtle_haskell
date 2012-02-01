module Graphics.X11.TurtleState (
	TurtleState(..),
	initialTurtleState,
) where

data TurtleState = TurtleState {
	shape :: [(Double, Double)],
	size :: Double,
	position :: (Double, Double),
	direction :: Double,
	pendown :: Bool,
	line :: Bool,
	undo :: Bool,
	undonum :: Int,
	clear :: Bool
 } deriving Show

initialTurtleState :: [(Double, Double)] -> TurtleState
initialTurtleState sh = TurtleState {
	shape = sh,
	size = 1,
	position = (0, 0),
	direction = 0,
	pendown = True,
	line = False,
	undo = False,
	undonum = 1,
	clear = False
 }
