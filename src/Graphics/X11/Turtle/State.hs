module Graphics.X11.Turtle.State (
	TurtleState(..),
	initialTurtleState,
) where

import Text.XML.YJSVG(SVG, Color(RGB))

data TurtleState = TurtleState {
	position :: (Double, Double),
	direction :: Double,
	degrees :: Double,
	pendown :: Bool,
	pensize :: Double,
	pencolor :: Color,
	shape :: [(Double, Double)],
	shapesize :: Double,
	visible :: Bool,
	clear :: Bool,
	undo :: Bool,
	undonum :: Int,
	draw :: Maybe SVG,
	drawed :: [SVG]
 } deriving Show

initialTurtleState :: [(Double, Double)] -> TurtleState
initialTurtleState sh = TurtleState {
	position = (0, 0),
	direction = 0,
	degrees = 360,
	pendown = True,
	pensize = 0,
	pencolor = RGB 0 0 0,
	shape = sh,
	shapesize = 1,
	visible = True,
	clear = False,
	undo = False,
	undonum = 1,
	draw = Nothing,
	drawed = []
 }
