module Graphics.X11.TurtleDraw (
	Field,
	Layer,
	Character,

	forkIOX,
	openField,
	addLayer,
	addCharacter,
	layerSize,

	turtleDraw,

	clearLayer
) where

import Graphics.X11.TurtleState(TurtleState(..))
import Graphics.X11.WindowLayers(
	Field, Layer, Character,
	forkIOX, openField, addLayer, addCharacter, layerSize, clearLayer,
	drawLine, drawCharacter, drawCharacterAndLine, undoLayer
 )

import Control.Concurrent(threadDelay)
import Control.Monad(when, forM_)
import Control.Arrow((***))

step :: Double
step = 10

moveSpeed :: Int
moveSpeed = 50000

stepDir :: Double
stepDir = 5

rotateSpeed :: Int
rotateSpeed = 10000

turtleDraw, turtleDrawNotUndo, turtleDrawUndo ::
	Character -> Layer -> TurtleState -> TurtleState -> IO ()
turtleDraw c l t0 t1 = if undo t1
	then turtleDrawUndo c l t0 t1
	else turtleDrawNotUndo c l t0 t1
turtleDrawUndo c l t0 t1 = do
	let	p0@(x0, y0) = position t0
		p1@(x1, y1) = position t1
		lineOrigin = if line t0 then Just p1 else Nothing
	when (line t0) $ undoLayer l
	forM_ (getDirections (direction t0) (direction t1)) $ \d -> do
		drawTurtle c (shape t1) (size t1) d p0 Nothing
		threadDelay rotateSpeed
	forM_ (getPoints x0 y0 x1 y1) $ \p -> do
		drawTurtle c (shape t1) (size t1) (direction t1) p lineOrigin
		threadDelay moveSpeed
	drawTurtle c (shape t1) (size t1) (direction t1) p1 lineOrigin
turtleDrawNotUndo c l t0 t1 = do
	let	p0@(x0, y0) = position t0
		p1@(x1, y1) = position t1
		lineOrigin = if line t1 then Just p0 else Nothing
	forM_ (getDirections (direction t0) (direction t1)) $ \d -> do
		drawTurtle c (shape t1) (size t1) d p0 Nothing
		threadDelay rotateSpeed
	forM_ (getPoints x0 y0 x1 y1) $ \p -> do
		drawTurtle c (shape t1) (size t1) (direction t1) p lineOrigin
		threadDelay moveSpeed
	drawTurtle c (shape t1) (size t1) (direction t1) p1 lineOrigin
	when (line t1) $ drawLine l x0 y0 x1 y1

getPoints :: Double -> Double -> Double -> Double -> [(Double, Double)]
getPoints x1 y1 x2 y2 = zip [x1, x1 + dx .. x2 - dx] [y1, y1 + dy .. y2 - dy]
	where
	len = ((x2 - x1) ** 2 + (y2 - y1) ** 2) ** (1/2)
	dx = (x2 - x1) * step / len
	dy = (y2 - y1) * step / len

getDirections :: Double -> Double -> [Double]
getDirections ds de = [ds, ds + dd .. de - dd]
	where
	dd = if de > ds then stepDir else - stepDir

drawTurtle :: Character -> [(Double, Double)] -> Double -> Double ->
	(Double, Double) -> Maybe (Double, Double) -> IO ()
drawTurtle c sh s d (px, py) org = do
	let sp = map (((+ px) *** (+ py)) . rotatePoint . ((* s) *** (* s))) sh
	maybe (drawCharacter c sp)
		(\(x0, y0) -> (drawCharacterAndLine c sp x0 y0 px py)) org
	where
	rotatePoint (x, y) = let rad = d * pi / 180 in
		(x * cos rad - y * sin rad, x * sin rad + y * cos rad)
