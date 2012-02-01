module Graphics.X11.TurtleMove (
	Field,
	Layer,
	Character,

	forkIOX,
	openField,
	closeField,
	addLayer,
	addCharacter,
	layerSize,

	moveTurtle
) where

import Graphics.X11.TurtleState(TurtleState(..))
import Graphics.X11.WindowLayers(
	Field, Layer, Character,
	forkIOX, openField, closeField,
	addLayer, addCharacter, layerSize, clearLayer,
	drawLine, drawCharacter, drawCharacterAndLine, undoLayer
 )

import Control.Concurrent(threadDelay)
import Control.Monad(when, forM_)
import Control.Arrow((***))

type Pos = (Double, Double)

step :: Double
step = 10

moveSpeed :: Int
moveSpeed = 50000

stepDir :: Double
stepDir = 5

rotateSpeed :: Int
rotateSpeed = 10000

moveTurtle :: Character -> Layer -> TurtleState -> TurtleState -> IO ()
moveTurtle c l t0 t1 = do
	when (undo t1 && line t0) $ undoLayer l
	forM_ (getDirections (direction t0) (direction t1)) $ \d -> do
		drawTurtle c (shape t1) (size t1) d p0 Nothing
		threadDelay rotateSpeed
	forM_ (getPositions x0 y0 x1 y1) $ \p -> do
		drawTurtle c (shape t1) (size t1) (direction t1) p lineOrigin
		threadDelay moveSpeed
	drawTurtle c (shape t1) (size t1) (direction t1) p1 lineOrigin
	when (not (undo t1) && line t1) $ drawLine l x0 y0 x1 y1
	when (clear t1) $ clearLayer l
	where
	(tl, to) = if undo t1 then (t0, t1) else (t1, t0)
	lineOrigin = if line tl then Just $ position to else Nothing
	p0@(x0, y0) = position t0
	p1@(x1, y1) = position t1

getPositions :: Double -> Double -> Double -> Double -> [Pos]
getPositions x0 y0 x1 y1 = zip [x0, x0 + dx .. x1 - dx] [y0, y0 + dy .. y1 - dy]
	where
	dist = ((x1 - x0) ** 2 + (y1 - y0) ** 2) ** (1/2)
	dx = step * (x1 - x0) / dist
	dy = step * (y1 - y0) / dist

getDirections :: Double -> Double -> [Double]
getDirections ds de = [ds, ds + dd .. de - dd]
	where
	dd = if de > ds then stepDir else - stepDir

drawTurtle :: Character -> [Pos] -> Double -> Double -> Pos -> Maybe Pos -> IO ()
drawTurtle c sh s d (px, py) org = do
	let sp = map (((+ px) *** (+ py)) . rotatePoint . ((* s) *** (* s))) sh
	maybe (drawCharacter c sp)
		(\(x0, y0) -> (drawCharacterAndLine c sp x0 y0 px py)) org
	where
	rotatePoint (x, y) = let rad = d * pi / 180 in
		(x * cos rad - y * sin rad, x * sin rad + y * cos rad)