module Graphics.X11.Turtle.State (
	TurtleState(..),
	initialTurtleState
) where

import Text.XML.YJSVG(SVG(Fill), Color(RGB))

data TurtleState = TurtleState {
	position :: (Double, Double),
	positionStep :: Maybe Double,
	direction :: Double,
	degrees :: Double,
	directionStep :: Maybe Double,
	interval :: Int,
	pendown :: Bool,
	fill :: Bool,
	pensize :: Double,
	pencolor :: Color,
	bgcolor :: Color,
	shape :: [(Double, Double)],
	shapesize :: (Double, Double),
	visible :: Bool,
	clear :: Bool,
	undo :: Bool,
	undonum :: Int,
	draw :: Maybe SVG,
	drawed :: [SVG],
	fillPoints :: [(Double, Double)]}
	deriving Show

initialTurtleState :: TurtleState
initialTurtleState = TurtleState {
	position = (0, 0),
	positionStep = Just 10,
	direction = 0,
	degrees = 360,
	directionStep = Just $ pi / 18,
	interval = 10000,
	pendown = True,
	fill = False,
	pensize = 1,
	pencolor = RGB 0 0 0,
	bgcolor = RGB 255 255 255,
	shape = [],
	shapesize = (1, 1),
	visible = True,
	clear = False,
	undo = False,
	undonum = 0,
	draw = Nothing,
	drawed = [Fill $ RGB 255 255 255],
	fillPoints = []}
