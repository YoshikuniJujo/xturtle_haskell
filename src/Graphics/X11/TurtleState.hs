module Graphics.X11.TurtleState (
	TurtleState(..),
	initialTurtleState,
	Color
) where

import Data.Word(Word32)

type Color = Word32

data TurtleState = TurtleState {
	shape :: [(Double, Double)],
	size :: Double,
	position :: (Double, Double),
	direction :: Double,
	pendown :: Bool,
	drawed :: [(Color, (Double, Double), (Double, Double))],
	line :: Bool,
	undo :: Bool,
	undonum :: Int,
	clear :: Bool,
	visible :: Bool,
	pencolor :: Color,
	pensize :: Int
 } deriving Show

initialTurtleState :: [(Double, Double)] -> TurtleState
initialTurtleState sh = TurtleState {
	shape = sh,
	size = 1,
	position = (0, 0),
	direction = 0,
	pendown = True,
	drawed = [],
	line = False,
	undo = False,
	undonum = 1,
	clear = False,
	visible = True,
	pencolor = 0x000000,
	pensize = 0
 }
