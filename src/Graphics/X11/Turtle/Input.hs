module Graphics.X11.Turtle.Input (
	TurtleState,
	TurtleInput(..),

	getTurtleStates,
	position,
	pendown,
	undonum,
	visible,
	direction,
	degrees,
	drawed,
) where

import Graphics.X11.Turtle.State(TurtleState(..), initialTurtleState)
import Control.Concurrent.Chan(Chan, newChan, getChanContents)
import Prelude hiding(Left)
import Text.XML.YJSVG(SVG(..), Color(..), Position(..))

data TurtleInput
	= Shape [(Double, Double)]
	| ShapeSize Double
	| Goto Double Double
	| Rotate Double
	| Penup
	| Pendown
	| SetVisible Bool
	| Undo
	| Clear
	| Forward Double
	| Left Double
	| Undonum Int
	| Pencolor Color
	| Pensize Double
	| Degrees Double
	| Write String Double String
	deriving (Show, Read)

getTurtleStates :: IO (Chan TurtleInput, [TurtleInput], [TurtleState])
getTurtleStates = do
	let	ts0 = initialTurtleState
	c <- newChan
	tis <- getChanContents c
	return (c, tis, ts0 : ts0 : inputToTurtle [] ts0 tis)

nextTurtle :: TurtleState -> TurtleInput -> TurtleState
nextTurtle t (Shape sh) = (clearState t){shape = sh}
nextTurtle t (ShapeSize ss) = (clearState t){shapesize = ss}
nextTurtle t (Goto x y) = (clearState t){position = (x, y),
	drawed = if pendown t then ln : drawed t else drawed t,
	draw = if pendown t then Just ln else Nothing}
	where
	ln = Line (uncurry Center $ position t) (Center x y) (pencolor t)
		(pensize t)
nextTurtle t (Rotate d) = (clearState t){direction = d}
nextTurtle t Pendown = (clearState t){pendown = True}
nextTurtle t Penup = (clearState t){pendown = False}
nextTurtle t (SetVisible v) = (clearState t){visible = v}
nextTurtle t (Undonum un) = (clearState t){undonum = un}
nextTurtle t (Clear) = (clearState t){clear = True, drawed = []}
nextTurtle t (Pencolor c) = (clearState t){pencolor = c}
nextTurtle t (Pensize ps) = (clearState t){pensize = ps}
nextTurtle t (Degrees ds) = (clearState t){
	degrees = ds,
	direction = direction t * ds / degrees t
 }
nextTurtle t (Write fnt sz str) = (clearState t){
	draw = Just d, drawed = d : drawed t
 }
	where
	(x, y) = position t
	d = Text (Center x y) sz (pencolor t) fnt str
nextTurtle _ _ = error "not defined"

clearState :: TurtleState -> TurtleState
clearState t = t{undo = False, undonum = 1, clear = False, draw = Nothing}

inputToTurtle :: [TurtleState] -> TurtleState -> [TurtleInput] -> [TurtleState]
inputToTurtle [] ts0 (Undo : tis) = ts0 : inputToTurtle [] ts0 tis
inputToTurtle (tsb : tsbs) _ (Undo : tis) =
	let ts1 = tsb{undo = True} in ts1 : inputToTurtle tsbs ts1 tis
inputToTurtle tsbs ts0 (Forward len : tis) = let
	(x0, y0) = position ts0
	dir = direction ts0 / degrees ts0
	x = x0 + len * cos (dir * 2 * pi)
	y = y0 + len * sin (dir * 2 * pi) in
	inputToTurtle tsbs ts0 $ Goto x y : tis
inputToTurtle tsbs ts0 (Left dd : tis) =
	inputToTurtle tsbs ts0 $ Rotate (direction ts0 + dd) : tis
inputToTurtle tsbs ts0 (ti : tis) =
	let ts1 = nextTurtle ts0 ti in ts1 : inputToTurtle (ts0 : tsbs) ts1 tis
inputToTurtle _ _ [] = error "no more input"