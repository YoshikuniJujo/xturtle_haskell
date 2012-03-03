module Graphics.X11.Turtle.State (
	TurtleState(..),
	initialTurtleState,
	getDirection,
	setDirection,
	getDirectionStep,
	setDirectionStep
) where

import Text.XML.YJSVG(SVG, Color(RGB))

getDirection :: TurtleState -> Double
getDirection ts = direction ts -- * (2 * pi) / degrees ts

setDirection :: TurtleState -> Double -> TurtleState
setDirection ts d = ts{direction = d * 2 * pi / degrees ts}

getDirectionStep :: TurtleState -> Maybe Double
getDirectionStep ts = directionStep ts -- (* ((2 * pi) / degrees ts)) `fmap` directionStep ts

setDirectionStep :: TurtleState -> Maybe Double -> TurtleState
setDirectionStep ts Nothing = ts{directionStep = Nothing}
setDirectionStep ts (Just ds) = ts{directionStep = Just $ ds * 2 * pi / degrees ts}

data TurtleState = TurtleState {
	position :: (Double, Double),
	positionStep :: Maybe Double,
	direction :: Double,
	degrees :: Double,
	directionStep :: Maybe Double,
	interval :: Int,
	pendown :: Bool,
	pensize :: Double,
	pencolor :: Color,
	shape :: [(Double, Double)],
	shapesize :: (Double, Double),
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
	direction = 0,
	degrees = 360,
	directionStep = Just $ pi / 18,
	interval = 10000,
	pendown = True,
	pensize = 1,
	pencolor = RGB 0 0 0,
	shape = [],
	shapesize = (1, 1),
	visible = True,
	clear = False,
	undo = False,
	undonum = 0,
	draw = Nothing,
	drawed = []}
