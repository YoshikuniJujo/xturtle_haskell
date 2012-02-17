module Graphics.X11.TurtleMove (
	Field,
	Layer,
	Character,

	forkIOX,
	openField,
	closeField,
	addLayer,
	addCharacter,
	fieldSize,
	clearLayer,
	clearCharacter,
	addThread,
	fieldColor,
	onclick,
	waitField,
	writeString,
	Color(..),

	moveTurtle
) where

import Graphics.X11.TurtleState(TurtleState(..), pencolor, Draw(..))
import Graphics.X11.TurtleField(
	Field, Layer, Character,
	forkIOX, openField, closeField, flushLayer,
	addLayer, addCharacter, fieldSize, clearLayer,
	drawLine, drawCharacter, drawCharacterAndLine, undoLayer,
	drawLineNotFlush,
	clearCharacter, addThread,
	fieldColor, onclick, waitField, writeString, Color(..)
 )

import Control.Concurrent(threadDelay)
import Control.Monad(when, unless, forM_)
import Control.Arrow((***))

type Pos = (Double, Double)

step :: Double
step = 10

moveSpeed :: Int
moveSpeed = 50000

stepDir :: Double
stepDir = 1 / 72

rotateSpeed :: Int
rotateSpeed = 10000

dir :: TurtleState -> Double
dir t = direction t / degrees t

moveTurtle :: Character -> Layer -> TurtleState -> TurtleState -> IO ()
moveTurtle c l t0 t1 = do
	when (undo t1 && (line t0 || draw t0 /= NoDraw)) $ do
		done <- undoLayer l
		unless done $ clearLayer l >> drawLines l (drawed t1)
	when (undo t1 && clear t0) $ drawLines l $ drawed t1
	when (visible t1) $ do
		forM_ (getDirections (dir t0) (dir t1)) $ \d -> do
			drawTurtle c (Color $ pencolor t1) (shape t1) (shapesize t1) d
				(pensize t1) p0 Nothing
			threadDelay rotateSpeed
		forM_ (getPositions x0 y0 x1 y1) $ \p -> do
			drawTurtle c (Color $ pencolor t1) (shape t1) (shapesize t1)
				(dir t1) (pensize t1) p lineOrigin
			threadDelay moveSpeed
		drawTurtle c (Color $ pencolor t1) (shape t1) (shapesize t1) (dir t1)
			(pensize t1) p1 lineOrigin
	unless (visible t1) $ clearCharacter c
	when (not (undo t1) && line t1) $
		drawLine l (pensize t1) (Color $ pencolor t1) x0 y0 x1 y1 >> flushLayer l
	when (clear t1) $ clearLayer l >> flushLayer l
	when (not $ undo t1) $ drawDraw l (draw t1) >> flushLayer l
	where
	(tl, to) = if undo t1 then (t0, t1) else (t1, t0)
	lineOrigin = if line tl then Just $ position to else Nothing
	p0@(x0, y0) = position t0
	p1@(x1, y1) = position t1

drawLines :: Layer -> [Draw] -> IO ()
drawLines l = mapM_ (drawDraw l) . reverse

drawDraw :: Layer -> Draw -> IO ()
drawDraw _ NoDraw = return ()
drawDraw l (Line clr lw (x0, y0) (x1, y1)) = drawLineNotFlush l lw (Color clr) x0 y0 x1 y1
drawDraw l (Str (r, g, b) fnt sz (x, y) str) = writeString l fnt sz r g b x y str

getPositions :: Double -> Double -> Double -> Double -> [Pos]
getPositions x0 y0 x1 y1 = take num $ zip [x0, x0 + dx .. ] [y0, y0 + dy .. ]
	where
	num = floor $ dist / step
	dist = ((x1 - x0) ** 2 + (y1 - y0) ** 2) ** (1/2)
	dx = step * (x1 - x0) / dist
	dy = step * (y1 - y0) / dist

getDirections :: Double -> Double -> [Double]
getDirections ds de = [ds, ds + dd .. de - dd]
	where
	dd = if de > ds then stepDir else - stepDir

drawTurtle :: Character -> Color -> [Pos] -> Double -> Double -> Double ->
	Pos -> Maybe Pos -> IO ()
drawTurtle c clr sh s d lw (px, py) org = do
	let sp = map (((+ px) *** (+ py)) . rotatePoint . ((* s) *** (* s))) sh
	maybe (drawCharacter c clr sp)
		(\(x0, y0) -> (drawCharacterAndLine c clr sp lw x0 y0 px py)) org
	where
	rotatePoint (x, y) = let rad = d * 2 * pi in
		(x * cos rad - y * sin rad, x * sin rad + y * cos rad)
