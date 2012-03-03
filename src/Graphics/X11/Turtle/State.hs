module Graphics.X11.Turtle.State (
	TurtleState(..),
	initialTurtleState,
) where

import Text.XML.YJSVG(SVG, Color(RGB))

data TurtleState = TurtleState {
	position :: (Double, Double),
	positionStep :: Maybe Double,
	positionInterval :: Int,
	direction :: Double,
	directionStep :: Maybe Double,
	directionInterval :: Int,
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
	drawed :: [SVG]}
	deriving Show

initialTurtleState :: TurtleState
initialTurtleState = TurtleState {
	position = (0, 0),
	positionStep = Just 10,
	positionInterval = 10000,
	direction = 0,
	directionStep = Just $ 1 / 36,
	directionInterval = 10000,
	degrees = 360,
	pendown = True,
	pensize = 1,
	pencolor = RGB 0 0 0,
	shape = [],
	shapesize = 1,
	visible = True,
	clear = False,
	undo = False,
	undonum = 0,
	draw = Nothing,
	drawed = []}
