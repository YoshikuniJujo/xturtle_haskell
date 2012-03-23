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
import Control.Arrow((***))
import Data.Tuple.Tools(rotate)

--------------------------------------------------------------------------------

data TurtleInput
	= Goto Position
	| Forward Double
	| Rotate Double
	| TurnLeft Double
	| Dot Double
	| Stamp
	| Write String Double String
	| PutImage FilePath Double Double
	| Undo
	| Undonum Int
	| Clear
	| Sleep Int
	| Flush
	| Shape [(Double, Double)]
	| Shapesize Double Double
	| Pensize Double
	| Pencolor Color
	| Bgcolor Color
	| SetPendown Bool
	| SetVisible Bool
	| SetFill Bool
	| SetPoly Bool
	| SetFlush Bool
	| PositionStep (Maybe Double)
	| DirectionStep (Maybe Double)
	| Degrees Double
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
inputToTurtle tsbs ts0 (Forward len : tis) = case position ts0 of
	Center x0 y0 -> let
		x = x0 + len * cos (direction ts0)
		y = y0 + len * sin (direction ts0) in
		inputToTurtle tsbs ts0 $ Goto (Center x y) : tis
	TopLeft x0 y0 -> let
		x = x0 + len * cos (direction ts0)
		y = y0 - len * sin (direction ts0) in
		inputToTurtle tsbs ts0 $ Goto (TopLeft x y) : tis
inputToTurtle tsbs ts0 (TurnLeft dd : tis) = inputToTurtle tsbs ts0 $
	Rotate (direction ts0 * degrees ts0 / (2 * pi) + dd) : tis
inputToTurtle tsbs ts0 (ti : tis) =
	let ts1 = nextTurtle ts0 ti in ts1 : inputToTurtle (ts0 : tsbs) ts1 tis
inputToTurtle _ _ [] = error "no more input"

clearState :: TurtleState -> TurtleState
clearState t = t{draw = Nothing, clear = False, undo = False, undonum = 1,
	sleep = Nothing, flush = False}

nextTurtle :: TurtleState -> TurtleInput -> TurtleState
nextTurtle t (Goto pos) = (clearState t){
	position = pos, draw = drw, drawed = maybe id (:) drw $ drawed t,
	fillPoints = (if fill t then (pos :) else id) $ fillPoints t,
	polyPoints = (if poly t then (pos :) else id) $ polyPoints t}
	where
	drw = if not $ pendown t then Nothing
		else Just $ Line pos (position t) (pencolor t) (pensize t)
nextTurtle t (Rotate dir) = (clearState t){direction = dir * 2 * pi / degrees t}
nextTurtle t (Dot sz) = (clearState t){
	draw = Just drw, drawed = drw : drawed t}
	where
	drw = Rect (position t) sz sz 0 (pencolor t) (pencolor t)
nextTurtle t Stamp = (clearState t){
	draw = Just drw, drawed = drw : drawed t}
	where
	Center x0 y0 = position t
	sp = let (sx, sy) = shapesize t in
		map (((+ x0) *** (+ y0)) . rotate (direction t) . ((* sx) *** (* sy))) $ shape t
	points = map (uncurry Center) sp
	drw = Polyline points (pencolor t) (pencolor t) 0
nextTurtle t (Write fnt sz str) = (clearState t){
	draw = Just txt, drawed = txt : drawed t}
	where txt = Text (position t) sz (pencolor t) fnt str
nextTurtle t (PutImage fp w h) = (clearState t){
	draw = Just img, drawed = img : drawed t}
	where img = Image (position t) w h fp
nextTurtle t (Undonum un) = (clearState t){undonum = un}
nextTurtle t Clear = (clearState t){clear = True, drawed = [last $ drawed t]}
nextTurtle t (Sleep time) = (clearState t){sleep = Just time}
nextTurtle t Flush = (clearState t){flush = True}
nextTurtle t (Shape sh) = (clearState t){shape = sh}
nextTurtle t (Shapesize sx sy) = (clearState t){shapesize = (sx, sy)}
nextTurtle t (Pensize ps) = (clearState t){pensize = ps}
nextTurtle t (Pencolor c) = (clearState t){pencolor = c}
nextTurtle t (Bgcolor c) = (clearState t){
	draw = Just $ Fill c, drawed = init (drawed t) ++ [Fill c]}
nextTurtle t (SetPendown pd) = (clearState t){pendown = pd}
nextTurtle t (SetVisible v) = (clearState t){visible = v}
nextTurtle t (SetFill f) = (clearState t){
	fill = f,
	draw = if fill t && not f then Just fl else Nothing,
	drawed = if fill t && not f then fl : drawed t else drawed t,
	fillPoints = [position t | f]}
	where
	fl = Polyline (fillPoints t) (pencolor t) (pencolor t) 0
nextTurtle t (SetPoly p) = (clearState t){
	poly = p,
	polyPoints = if p then [position t] else polyPoints t}
nextTurtle t (SetFlush ss) = (clearState t){stepbystep = ss}
nextTurtle t (PositionStep ps) = (clearState t){positionStep = ps}
nextTurtle t (DirectionStep ds) = (clearState t){directionStep = ds}
nextTurtle t (Degrees ds) = (clearState t){degrees = ds}
nextTurtle _ _ = error "not defined"
