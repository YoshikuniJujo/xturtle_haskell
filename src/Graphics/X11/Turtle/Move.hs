module Graphics.X11.Turtle.Move(
	-- * types
	Field,
	Coordinates(..),

	-- * process Field
	openField,
	closeField,
	waitField,
	topleft,
	center,
	coordinates,
	fieldSize,

	-- * draws
	forkField,
	flushField,
	addLayer,
	clearLayer,
	addCharacter,
	clearCharacter,
	moveTurtle,

	-- * event
	onclick,
	onrelease,
	ondrag,
	onmotion,
	onkeypress,
	ontimer
) where

import Graphics.X11.Turtle.State(TurtleState(..), makeShape)
import Graphics.X11.Turtle.Field(
	Field, Layer, Character, Coordinates(..),
	openField, closeField, waitField, coordinates, topleft, center,
	fieldSize, forkField, flushField, addLayer, clearLayer,
	addCharacter, clearCharacter,
	onclick, onrelease, ondrag, onmotion, onkeypress, ontimer,
	fieldColor, drawLine, fillRectangle, fillPolygon, writeString,
	drawImage, undoLayer, drawCharacter, drawCharacterAndLine)
import Text.XML.YJSVG(SVG(..), Position(..))
import qualified Text.XML.YJSVG as S(topleft)

import Control.Concurrent(threadDelay)
import Control.Monad(when, unless, forM_, replicateM_)
import Control.Monad.Tools(unlessM)
import Data.Maybe(isJust, isNothing)

--------------------------------------------------------------------------------

moveTurtle :: Field -> Character -> Layer -> TurtleState -> TurtleState -> IO ()
moveTurtle _ _ _ _ TurtleState{sleep = Just t} = threadDelay $ 1000 * t
moveTurtle f _ _ _ TurtleState{flush = True} = flushField f True $ return ()
moveTurtle f c l t0 t1 = do
	(w, h) <- fieldSize f
	when (undo t1) $ fl $ do
		when (clear t0) redraw
		when (isJust $ draw t0) $ do
			unlessM (undoLayer l) $ clearLayer l >> redraw
			when (visible t1) $ drawTtl (direction t0) $ position t0
	case silentundo t1 of
		Just n -> replicateM_ n (undoLayer l) >> clearLayer l >> redraw
		_ -> return ()
	when (visible t1 && isNothing (silentundo t1)) $ do
		forM_ (directions t0 t1) $ \dir -> fl $
			drawTtl dir (position t0) >> threadDelay (interval t0)
		forM_ (positions w h t0 t1) $ \p -> fl $
			drawTtl (direction t1) p >> threadDelay (interval t0)
		fl $ drawTtl (direction t1) $ position t1
	when (visible t0 && not (visible t1)) $ fl $ clearCharacter c
	when (clear t1) $ fl $ clearLayer l
	unless (undo t1 || isJust (silentundo t1)) $
		fl $ maybe (return ()) (drawSVG f l) (draw t1)
	where
	fl = flushField f $ stepbystep t0
	redraw = mapM_ (drawSVG f l) $ reverse $ drawed t1
	drawTtl dir pos = drawTurtle f c t1 dir pos begin
	begin	| undo t1 && pendown t0 = Just $ position t1
		| pendown t1 = Just $ position t0
		| otherwise = Nothing

drawSVG :: Field -> Layer -> SVG -> IO ()
drawSVG f l (Line p0 p1 clr lw) = drawLine f l lw clr p0 p1
drawSVG f l (Rect pos w h 0 fc _) = fillRectangle f l pos w h fc
drawSVG f l (Polyline ps fc _ 0) = fillPolygon f l ps fc
drawSVG f l (Fill clr) = fieldColor f l clr
drawSVG f l (Text pos sz clr fnt str) = writeString f l fnt sz clr pos str
drawSVG f l (Image pos w h fp) = drawImage f l fp pos w h
drawSVG _ _ _ = error "not implemented"

positions :: Double -> Double -> TurtleState -> TurtleState -> [Position]
positions w h t0 t1 =
	maybe [] (mkPositions w h (position t0) (position t1)) $ positionStep t0

mkPositions :: Double -> Double -> Position -> Position -> Double -> [Position]
mkPositions w h p1 p2 step = case (p1, p2) of
	(Center x0 y0, Center x1 y1) -> map (uncurry Center) $ mp x0 y0 x1 y1
	(TopLeft x0 y0, TopLeft x1 y1) -> map (uncurry TopLeft) $ mp x0 y0 x1 y1
	_ -> mkPositions w h (S.topleft w h p1) (S.topleft w h p2) step
	where
	mp x0 y0 x1 y1 = let dist = ((x1 - x0) ** 2 + (y1 - y0) ** 2) ** (1 / 2)
		in take (floor $ dist / step) $ zip
			[x0, x0 + step * (x1 - x0) / dist .. ]
			[y0, y0 + step * (y1 - y0) / dist .. ]

directions :: TurtleState -> TurtleState -> [Double]
directions t0 t1 = case directionStep t0 of
	Nothing -> []
	Just step -> [ds, ds + dd .. de - dd]
		where
		dd = if de > ds then step else - step
		ds = direction t0
		de = direction t1

drawTurtle :: Field -> Character -> TurtleState -> Double -> Position ->
	Maybe Position -> IO ()
drawTurtle f c ts@TurtleState{pencolor = clr} dir pos = maybe
	(drawCharacter f c clr $ makeShape ts dir pos)
	(drawCharacterAndLine f c clr (makeShape ts dir pos) (pensize ts) pos)
