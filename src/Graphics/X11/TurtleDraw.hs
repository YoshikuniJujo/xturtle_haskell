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
getPoints x1 y1 x2 y2 = let
        len = ((x2 - x1) ** 2 + (y2 - y1) ** 2) ** (1/2)
        dx = (x2 - x1) * step / len
        dy = (y2 - y1) * step / len in
        zip (takeWhile (before dx x2) [x1, x1 + dx ..])
                (takeWhile (before dy y2) [y1, y1 + dy ..])

before :: (Num a, Ord a) => a -> a -> a -> Bool
before d t x = signum d * t >= signum d * x

getDirections :: Double -> Double -> [Double]
getDirections ds de = takeWhile beforeDir [ds, ds + dd ..] ++ [de]
        where
        sig = signum (de - ds)
        dd = sig * stepDir
        beforeDir x = sig * x < sig * de

drawTurtle :: Character -> [(Double, Double)] -> Double -> Double ->
	(Double, Double) -> Maybe (Double, Double) -> IO ()
drawTurtle c sh s d (x, y) org = do
	let sp = mkShape sh s d x y
	maybe (drawCharacter c sp)
		(\(x0, y0) -> (drawCharacterAndLine c sp x0 y0 x y)) org

mkShape ::
	[(Double, Double)] -> Double -> Double -> Double -> Double -> [(Double, Double)]
mkShape sh s d x y =
	map (uncurry (addDoubles (x, y)) . rotatePointD d . mulPoint s) sh

addDoubles :: (Double, Double) -> Double -> Double -> (Double, Double)
addDoubles (x, y) dx dy = (x + dx, y + dy)

rotatePointD :: Double -> (Double, Double) -> (Double, Double)
rotatePointD = rotatePointR . (* pi) . (/ 180)

rotatePointR :: Double -> (Double, Double) -> (Double, Double)
rotatePointR rad (x, y) =
	(x * cos rad - y * sin rad, x * sin rad + y * cos rad)

mulPoint :: Double -> (Double, Double) -> (Double, Double)
mulPoint s (x, y) = (x * s, y * s)
