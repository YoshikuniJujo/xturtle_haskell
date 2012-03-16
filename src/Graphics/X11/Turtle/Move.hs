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

	-- * draws
	flushField,
	moveTurtle,
	addLayer,
	clearLayer,
	addCharacter,
	clearCharacter,

	-- * event
	onclick,
	onrelease,
	ondrag,
	onmotion,
	onkeypress,
	ontimer
) where

import Graphics.X11.Turtle.State(TurtleState(..))
import Graphics.X11.Turtle.Field(
	Field, Layer, Character,
	openField, closeField, waitField, fieldSize,
	forkField, flushField, fieldColor,
	addLayer, drawLine, writeString, undoLayer, clearLayer, fillPolygon,
	fillRectangle,
	addCharacter, drawCharacter, drawCharacterAndLine, clearCharacter,
	onclick, onrelease, ondrag, onmotion, onkeypress, drawImage, ontimer)
import Text.XML.YJSVG(SVG(..), Position(..))

import Control.Concurrent(threadDelay)
import Control.Monad(when, unless, forM_)
import Control.Monad.Tools(unlessM)
import Control.Arrow((***))
import Data.Maybe(isJust)
import Data.Tuple.Tools(rotate)

--------------------------------------------------------------------------------

moveTurtle :: Field -> Character -> Layer -> TurtleState -> TurtleState -> IO ()
moveTurtle _ _ _ _ TurtleState{sleep = Just t} = threadDelay $ 1000 * t
moveTurtle f _ _ _ TurtleState{flush = True} = flushField f True $ return ()
moveTurtle f c l t0 t1 = do
	when (undo t1) $ flushField f fl $ do
		when (clear t0) redraw
		when (isJust $ draw t0) $ do
			unlessM (undoLayer l) $ clearLayer l >> redraw
			when (visible t1) $ drawT (direction t1) $ position t0
	when (visible t1) $ do
		forM_ (directions t0 t1) $ \dir -> flushField f fl $
			drawT dir (position t0) >> threadDelay (interval t0)
		forM_ (positions t0 t1) $ \p -> flushField f fl $
			drawT (direction t1) (uncurry Center p) >> threadDelay (interval t0)
		flushField f fl $ drawT (direction t1) $ position t1
	when (bgcolor t0 /= bgcolor t1) $
		flushField f fl $ fieldColor f l $ bgcolor t1
	unless (undo t1) $ flushField f fl $ do
		when (visible t0 && not (visible t1)) $ clearCharacter c
		when (clear t1) $ clearLayer l
		maybe (return ()) (drawSVG f l) (draw t1)
	where
	fl = stepbystep t0
	redraw = mapM_ (drawSVG f l) $ reverse $ drawed t1
	drawT d p = drawTurtle f c t1 d p lineOrigin
	lineOrigin	| undo t1 && pendown t0 = Just $ position t1
			| pendown t1 = Just $ position t0
			| otherwise = Nothing

drawSVG :: Field -> Layer -> SVG -> IO ()
drawSVG f l (Line p0 p1 clr lw) = drawLine f l lw clr p0 p1
drawSVG f l (Text pos sz clr fnt str) = writeString f l fnt sz clr pos str
drawSVG f l (Polyline ps fc _ 0) = fillPolygon f l ps fc
drawSVG f l (Rect pos w h 0 fc _) = fillRectangle f l pos w h fc
drawSVG f l (Image pos w h fp) = drawImage f l fp pos w h
drawSVG _ _ (Fill _) = return ()
drawSVG _ _ _ = error "not implemented"

positions :: TurtleState -> TurtleState -> [(Double, Double)]
positions t0 t1 = case positionStep t0 of
	Nothing -> []
	Just step -> take (floor $ dist / step) $ zip
		[x0, x0 + step * (x1 - x0) / dist .. ]
		[y0, y0 + step * (y1 - y0) / dist .. ]
	where
	[Center x0 y0, Center x1 y1] = map position [t0, t1]
	dist = ((x1 - x0) ** 2 + (y1 - y0) ** 2) ** (1/2)

directions :: TurtleState -> TurtleState -> [Double]
directions t0 t1 = case directionStep t0 of
	Nothing -> []
	Just step -> let dd = if de > ds then step else - step in
		[ds, ds + dd .. de - dd]
	where
	ds = direction t0
	de = direction t1

drawTurtle :: Field -> Character -> TurtleState -> Double ->
	Position -> Maybe Position -> IO ()
drawTurtle f c t d p0@Center{posX = px, posY = py} = maybe (drawCharacter f c (pencolor t) sp)
	(drawCharacterAndLine f c (pencolor t) sp (pensize t) p0)
	where
	sp = let (sx, sy) = shapesize t in
		map (uncurry Center . ((+ px) *** (+ py)) . rotate d . ((* sx) *** (* sy))) $ shape t
