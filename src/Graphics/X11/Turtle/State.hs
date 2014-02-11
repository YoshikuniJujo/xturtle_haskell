module Graphics.X11.Turtle.State(
	TurtleState(..), initTurtleState, makeShape) where

import Text.XML.YJSVG(Position(..), SVG(Fill), Color(RGB))
import Data.Tuple.Tools(rotate)
import Control.Arrow((***))
import Control.Concurrent.Chan (Chan)

data TurtleState = TurtleState {
	position :: Position,
	direction :: Double,
	degrees :: Double,
	shape :: [(Double, Double)],
	shapesize :: (Double, Double),
	pensize :: Double,
	pencolor :: Color,
	pendown :: Bool,
	visible :: Bool,
	stepbystep :: Bool,

	draw :: Maybe SVG,
	drawed :: [SVG],
	clear :: Bool,
	undo :: Bool,
	undonum :: Int,
	silentundo :: Maybe Int,
	sleep :: Maybe Int,
	flush :: Bool,
	fill :: Bool,
	poly :: Bool,
	fillPoints :: [Position],
	polyPoints :: [Position],

	positionStep :: Maybe Double,
	directionStep :: Maybe Double,
	interval :: Int,

	finishSign :: Maybe (Chan ())
 }

initTurtleState :: TurtleState
initTurtleState = TurtleState {
	position = Center 0 0,
	direction = 0,
	degrees = 360,
	shape = [],
	shapesize = (1, 1),
	pensize = 1,
	pencolor = RGB 0 0 0,
	pendown = True,
	visible = True,
	stepbystep = True,

	draw = Nothing,
	drawed = [Fill $ RGB 255 255 255],
	clear = False,
	undo = False,
	undonum = 0,
	silentundo = Nothing,
	sleep = Nothing,
	flush = False,
	fill = False,
	poly = False,
	fillPoints = [],
	polyPoints = [],

	positionStep = Just 10,
	directionStep = Just $ pi / 18,
	interval = 10000,

	finishSign = Nothing
 }

makeShape :: TurtleState -> Double -> Position -> [Position]
makeShape ts dir_ pos = (mkPos . move . rotate dir . resize) `map` shape ts
	where
	move = (+ posX pos) *** (+ posY pos)
	resize = uncurry (***) $ ((*) *** (*)) $ shapesize ts
	(mkPos, dir) = case pos of
		Center{} -> (uncurry Center, dir_)
		TopLeft{} -> (uncurry TopLeft, - dir_)
