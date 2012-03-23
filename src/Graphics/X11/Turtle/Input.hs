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

import Graphics.X11.Turtle.State(TurtleState(..), initialTurtleState, makeShape)
import Text.XML.YJSVG(SVG(..), Color(..), Position(..))
import Control.Concurrent.Chan(Chan, newChan, getChanContents)

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

reset :: TurtleState -> TurtleState
reset t = t{draw = Nothing, clear = False, undo = False, undonum = 1,
	sleep = Nothing, flush = False}

set :: TurtleState -> Maybe SVG -> TurtleState
set t drw = t{draw = drw, drawed = maybe id (:) drw $ drawed t}

nextTurtle :: TurtleState -> TurtleInput -> TurtleState
nextTurtle t (Goto pos) = (reset t){position = pos,
	fillPoints = (if fill t then (pos :) else id) $ fillPoints t,
	polyPoints = (if poly t then (pos :) else id) $ polyPoints t}
	`set` if not $ pendown t then Nothing
		else Just $ Line pos (position t) (pencolor t) (pensize t)
nextTurtle t (Rotate dir) = (reset t){direction = dir * 2 * pi / degrees t}
nextTurtle t (Dot sz) =
	reset t `set` Just (Rect (position t) sz sz 0 (pencolor t) (pencolor t))
nextTurtle t@TurtleState{pencolor = clr} Stamp = reset t `set`
	Just (Polyline (makeShape t (direction t) (position t)) clr clr 0)
nextTurtle t (Write fnt sz str) =
	reset t `set` Just (Text (position t) sz (pencolor t) fnt str)
nextTurtle t (PutImage fp w h) = reset t `set` Just (Image (position t) w h fp)
nextTurtle t (Undonum un) = (reset t){undonum = un}
nextTurtle t Clear = (reset t){clear = True, drawed = [last $ drawed t]}
nextTurtle t (Sleep time) = (reset t){sleep = Just time}
nextTurtle t Flush = (reset t){flush = True}
nextTurtle t (Shape sh) = (reset t){shape = sh}
nextTurtle t (Shapesize sx sy) = (reset t){shapesize = (sx, sy)}
nextTurtle t (Pensize ps) = (reset t){pensize = ps}
nextTurtle t (Pencolor clr) = (reset t){pencolor = clr}
nextTurtle t (Bgcolor clr) = (reset t){
	draw = Just $ Fill clr, drawed = init (drawed t) ++ [Fill clr]}
nextTurtle t (SetPendown pd) = (reset t){pendown = pd}
nextTurtle t (SetVisible v) = (reset t){visible = v}
nextTurtle t (SetFill fl) = (reset t){fill = fl, fillPoints = [position t | fl]}
	`set` (if not (fill t) || fl then Nothing
		else Just $ Polyline (fillPoints t) (pencolor t) (pencolor t) 0)
nextTurtle t (SetPoly p) = (reset t){
	poly = p, polyPoints = if p then [position t] else polyPoints t}
nextTurtle t (SetFlush ss) = (reset t){stepbystep = ss}
nextTurtle t (PositionStep ps) = (reset t){positionStep = ps}
nextTurtle t (DirectionStep ds) = (reset t){directionStep = ds}
nextTurtle t (Degrees ds) = (reset t){degrees = ds}
nextTurtle _ _ = error "not defined"
