module Graphics.X11.Turtle.Move (
	-- * types
	Field,
	Layer,
	Character,

	-- * process Field
	openField,
	closeField,
	forkField,
	waitField,
	fieldSize,
	fieldColor,

	-- * draws
	moveTurtle,
	addLayer,
	clearLayer,
	addCharacter,
	clearCharacter,

	-- * event
	onclick,
	onrelease,
	ondrag,
	onkeypress
) where

import Graphics.X11.Turtle.State(TurtleState(..))
import Graphics.X11.Turtle.Field(
	Field, Layer, Character,
	openField, closeField, waitField, fieldSize,
	forkField, flushField, fieldColor,
	addLayer, drawLine, writeString, undoLayer, clearLayer,
	addCharacter, drawCharacter, drawCharacterAndLine, clearCharacter,
	onclick, onrelease, ondrag, onkeypress)
import Text.XML.YJSVG(SVG(..), Position(..), Color)

import Control.Concurrent(threadDelay)
import Control.Monad(when, unless, forM_)
import Control.Monad.Tools(unlessM)
import Control.Arrow((***))
import Data.Maybe(isJust)

--------------------------------------------------------------------------------

dir :: TurtleState -> Double
dir t = direction t / degrees t

moveTurtle :: Field -> Character -> Layer -> TurtleState -> TurtleState -> IO ()
moveTurtle f c l t0 t1 = do
	when (undo t1 && clear t0) $
		flushField f $ mapM_ (drawSVG f l) $ reverse $ drawed t1
	when (undo t1 && isJust (draw t0)) $ flushField f $ do
		unlessM (undoLayer l) $ clearLayer l >>
			mapM_ (drawSVG f l) (reverse $ drawed t1)
		when (visible t1) $
			drawTurtle f c (pencolor t1) (shape t1) (shapesize t1)
				(dir t1) (pensize t1) (position t0) lineOrigin
	when (visible t1) $ do
		forM_ (directions (directionStep t0) (dir t0) (dir t1)) $ \d -> flushField f $ do
			drawTurtle f c (pencolor t1) (shape t1) (shapesize t1) d
				(pensize t1) (position t0) Nothing
			threadDelay $ directionInterval t0
		forM_ (positions (positionStep t0) (position t0) (position t1)) $ \p -> flushField f $ do
			drawTurtle f c (pencolor t1) (shape t1) (shapesize t1)
				(dir t1) (pensize t1) p lineOrigin
			threadDelay $ positionInterval t0
		flushField f $
			drawTurtle f c (pencolor t1) (shape t1) (shapesize t1)
				(dir t1) (pensize t1) (position t1) lineOrigin
	unless (visible t1) $ clearCharacter c
	when (clear t1) $ flushField f $ clearLayer l
	unless (undo t1) $ flushField f $ maybe (return ()) (drawSVG f l) (draw t1)
	where
	(tl, to) = if undo t1 then (t0, t1) else (t1, t0)
	lineOrigin = if pendown tl then Just $ position to else Nothing

drawSVG :: Field -> Layer -> SVG -> IO ()
drawSVG f l (Line (Center x0 y0) (Center x1 y1) clr lw) =
	drawLine f l lw clr x0 y0 x1 y1
drawSVG f l (Text (Center x y) sz clr fnt str) =
	writeString f l fnt sz clr x y str
drawSVG _ _ _ = error "not implemented"

positions :: Maybe Double -> (Double, Double) -> (Double, Double) -> [(Double, Double)]
positions Nothing _ _ = []
positions (Just step) (x0, y0) (x1, y1) = take num $ zip [x0, x0 + dx .. ] [y0, y0 + dy .. ]
	where
	num = floor $ dist / step
	dist = ((x1 - x0) ** 2 + (y1 - y0) ** 2) ** (1/2)
	dx = step * (x1 - x0) / dist
	dy = step * (y1 - y0) / dist

directions :: Maybe Double -> Double -> Double -> [Double]
directions Nothing _ _ = []
directions (Just step) ds de =
	let dd = if de > ds then step else - step in [ds, ds + dd .. de - dd]

drawTurtle :: Field -> Character -> Color -> [(Double, Double)] -> Double ->
	Double -> Double -> (Double, Double) -> Maybe (Double, Double) -> IO ()
drawTurtle f c clr sh s d lw (px, py) org = maybe (drawCharacter f c clr sp)
	(uncurry $ drawCharacterAndLine f c clr sp lw px py) org
	where
	sp = map (((+ px) *** (+ py)) . rotate . ((* s) *** (* s))) sh
	rad = d * 2 * pi
	rotate (x, y) = (x * cos rad - y * sin rad, x * sin rad + y * cos rad)
