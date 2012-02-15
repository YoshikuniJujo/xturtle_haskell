module Graphics.X11.TurtleState (
	TurtleState(..),
	initialTurtleState,
	Color
) where

import Data.Word(Word32)

type Color = Word32

data TurtleState = TurtleState {
	position :: (Double, Double),
	direction :: Double,
	degrees :: Double,
	pendown :: Bool,
	pensize :: Int,
	pencolor :: Color,
	shape :: [(Double, Double)],
	shapesize :: Double,
	visible :: Bool,
	clear :: Bool,
	undo :: Bool,
	line :: Bool,
	undonum :: Int,
	drawed :: [(Color, (Double, Double), (Double, Double))]
 } deriving Show

initialTurtleState :: [(Double, Double)] -> TurtleState
initialTurtleState sh = TurtleState {
	position = (0, 0),
	direction = 0,
	degrees = 360,
	pendown = True,
	pensize = 0,
	pencolor = 0x000000,
	shape = sh,
	shapesize = 1,
	visible = True,
	clear = False,
	undo = False,
	line = False,
	undonum = 1,
	drawed = []
 }
