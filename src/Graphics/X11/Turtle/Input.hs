module Graphics.X11.Turtle.Input(
	-- * types
	TurtleState,
	TurtleInput(..),

	-- * get TurtlsStates in timeline
	turtleSeries,

	-- * read TurtleState members
	position,
	direction,
	degrees,
	pendown,
	visible,
	undonum,
	drawed,
	polyPoints
) where

import Graphics.X11.Turtle.State(TurtleState(..), initialTurtleState)
import Text.XML.YJSVG(SVG(..), Color(..), Position(..))

import Control.Concurrent.Chan(Chan, newChan, getChanContents)

--------------------------------------------------------------------------------

data TurtleInput
	= Shape [(Double, Double)]
	| Shapesize Double Double
	| Pencolor Color
	| Pensize Double
	| SetPendown Bool
	| SetFill Bool
	| SetPoly Bool
	| SetVisible Bool
	| Degrees Double
	| PositionStep (Maybe Double)
	| DirectionStep (Maybe Double)
	| Undonum Int
	| Goto Double Double
	| Rotate Double
	| Write String Double String
	| PutImage FilePath Double Double
	| Bgcolor Color
	| Undo
	| Clear
	| Forward Double
	| TurnLeft Double
	| Sleep Int
	| SetFlush Bool
	| Flush
	deriving (Show, Read)

turtleSeries :: IO (Chan TurtleInput, [TurtleInput], [TurtleState])
turtleSeries = do
	let	ts0 = initialTurtleState
	c <- newChan
	tis <- getChanContents c
	return (c, tis, ts0 : ts0 : inputToTurtle [] ts0 tis)

inputToTurtle :: [TurtleState] -> TurtleState -> [TurtleInput] -> [TurtleState]
inputToTurtle [] ts0 (Undo : tis) = ts0 : inputToTurtle [] ts0 tis
inputToTurtle (tsb : tsbs) _ (Undo : tis) =
	let ts1 = tsb{undo = True} in ts1 : inputToTurtle tsbs ts1 tis
inputToTurtle tsbs ts0 (Forward len : tis) = let
	(x0, y0) = position ts0
	x = x0 + len * cos (direction ts0)
	y = y0 + len * sin (direction ts0) in
	inputToTurtle tsbs ts0 $ Goto x y : tis
inputToTurtle tsbs ts0 (TurnLeft dd : tis) = inputToTurtle tsbs ts0 $
	Rotate (direction ts0 * degrees ts0 / (2 * pi) + dd) : tis
inputToTurtle tsbs ts0 (ti : tis) =
	let ts1 = nextTurtle ts0 ti in ts1 : inputToTurtle (ts0 : tsbs) ts1 tis
inputToTurtle _ _ [] = error "no more input"

nextTurtle :: TurtleState -> TurtleInput -> TurtleState
nextTurtle t (Shape sh) = (clearState t){shape = sh}
nextTurtle t (Shapesize sx sy) = (clearState t){shapesize = (sx, sy)}
nextTurtle t (Pencolor c) = (clearState t){pencolor = c}
nextTurtle t (Pensize ps) = (clearState t){pensize = ps}
nextTurtle t (SetPendown pd) = (clearState t){pendown = pd}
nextTurtle t (SetFill f) = (clearState t){
	fill = f,
	draw = if fill t && not f then Just fl else Nothing,
	drawed = if fill t && not f then fl : drawed t else drawed t,
	fillPoints = [(x0, y0) | f]}
	where
	(x0, y0) = position t
	fl = Polyline (uncurry Center `map` fillPoints t)
		(pencolor t) (pencolor t) 0
nextTurtle t (SetPoly p) = (clearState t){
	poly = p,
	polyPoints = if p then [position t] else polyPoints t}
nextTurtle t (SetVisible v) = (clearState t){visible = v}
nextTurtle t (Degrees ds) = (clearState t){degrees = ds}
nextTurtle t (PositionStep ps) = (clearState t){positionStep = ps}
nextTurtle t (DirectionStep ds) = (clearState t){directionStep = ds}
nextTurtle t (Undonum un) = (clearState t){undonum = un}
nextTurtle t (Goto x y) = (clearState t){
	position = (x, y),
	draw = if pendown t then Just ln else Nothing,
	drawed = if pendown t then ln : drawed t else drawed t,
	fillPoints = if fill t then (x, y) : fillPoints t else fillPoints t,
	polyPoints = if poly t then (x, y) : polyPoints t else polyPoints t}
	where
	(x0, y0) = position t
	ln = Line (Center x0 y0) (Center x y) (pencolor t) (pensize t)
nextTurtle t (Rotate d) = (clearState t){direction = d * 2 * pi / degrees t}
nextTurtle t (Write fnt sz str) = (clearState t){
	draw = Just txt, drawed = txt : drawed t}
	where txt = Text (uncurry Center $ position t) sz (pencolor t) fnt str
nextTurtle t (PutImage fp w h) = (clearState t){
	draw = Just img, drawed = img : drawed t}
	where img = Image (uncurry Center $ position t) w h fp
nextTurtle t (Bgcolor c) = (clearState t){
	bgcolor = c, drawed = init (drawed t) ++ [Fill c]}
nextTurtle t Clear = (clearState t){clear = True, drawed = [last $ drawed t]}
nextTurtle t (Sleep time) = (clearState t){sleep = Just time}
nextTurtle t (SetFlush ss) = (clearState t){stepbystep = ss}
nextTurtle t Flush = (clearState t){flush = True}
nextTurtle _ _ = error "not defined"

clearState :: TurtleState -> TurtleState
clearState t = t{undonum = 1, undo = False, clear = False, draw = Nothing,
	sleep = Nothing, flush = False}
