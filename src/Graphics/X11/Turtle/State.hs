module Graphics.X11.Turtle.State(TurtleState(..), initialTurtleState) where

import Text.XML.YJSVG(Position(..), SVG(Fill), Color(RGB))

data TurtleState = TurtleState {
	position :: Position,
	direction :: Double,
	degrees :: Double,
	shape :: [(Double, Double)],
	shapesize :: (Double, Double),
	pensize :: Double,
	pencolor :: Color,
	bgcolor :: Color,
	pendown :: Bool,
	visible :: Bool,
	stepbystep :: Bool,

	draw :: Maybe SVG,
	drawed :: [SVG],
	clear :: Bool,
	undo :: Bool,
	undonum :: Int,
	sleep :: Maybe Int,
	flush :: Bool,
	fill :: Bool,
	poly :: Bool,
	fillPoints :: [Position],
	polyPoints :: [Position],

	positionStep :: Maybe Double,
	directionStep :: Maybe Double,
	interval :: Int}

initialTurtleState :: TurtleState
initialTurtleState = TurtleState {
	position = Center 0 0,
	direction = 0,
	degrees = 360,
	shape = [],
	shapesize = (1, 1),
	pensize = 1,
	pencolor = RGB 0 0 0,
	bgcolor = RGB 255 255 255,
	pendown = True,
	visible = True,
	stepbystep = True,

	draw = Nothing,
	drawed = [Fill $ RGB 255 255 255],
	clear = False,
	undo = False,
	undonum = 0,
	sleep = Nothing,
	flush = False,
	fill = False,
	poly = False,
	fillPoints = [],
	polyPoints = [],

	positionStep = Just 10,
	directionStep = Just $ pi / 18,
	interval = 10000}
