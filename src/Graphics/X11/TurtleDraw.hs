module Graphics.X11.TurtleDraw (
	Field,
	Layer,
	Character,

	openField,
	addLayer,
	addCharacter,
	clearLayer,
	turtleDraw,

	winSize
) where

import Graphics.X11.TurtleState
import Graphics.X11.DrawTurtle
import Control.Concurrent
import Control.Monad

turtleDraw, turtleDrawNotUndo, turtleDrawUndo ::
	Field -> Character -> Layer -> TurtleState -> TurtleState -> IO ()
turtleDraw f c l t0 t1 = do
	let	isUndo = turtleUndo t1
	if isUndo then turtleDrawUndo f c l t0 t1
		else turtleDrawNotUndo f c l t0 t1
turtleDrawUndo f c l t0 t1 = do
	let	shape = turtleShape t1
		size = turtleSize t1
		prePos@(px, py) = turtlePos t0
		preDir = turtleDir t0
		pos@(nx, ny) = turtlePos t1
		dir = turtleDir t1
--		doneLine = turtleLineDone t0
--	when doneLine $ undoLayer f l
	undoLayer f l
	forM_ (getDirections preDir dir) $ \d ->
		drawTurtle f c shape size d prePos Nothing
	forM_ (getPoints px py nx ny) $ \p -> do
		drawTurtle f c shape size dir p $ Just pos
		threadDelay 50000
turtleDrawNotUndo f c l t0 t1 = do
	let	shape = turtleShape t1
		size = turtleSize t1
		prePos@(px, py) = turtlePos t0
		preDir = turtleDir t0
		(nx, ny) = turtlePos t1
		dir = turtleDir t1
	forM_ (getDirections preDir dir) $ \d ->
		drawTurtle f c shape size d prePos Nothing
	forM_ (getPoints px py nx ny) $ \p -> do
		drawTurtle f c shape size dir p $ Just prePos
		threadDelay 50000
	line f l px py nx ny

step :: Double
step = 10
stepDir :: Double
stepDir = 5

getPoints :: Double -> Double -> Double -> Double -> [(Double, Double)]
getPoints x1 y1 x2 y2 = let
        len = ((x2 - x1) ** 2 + (y2 - y1) ** 2) ** (1/2)
        dx = (x2 - x1) * step / len
        dy = (y2 - y1) * step / len in
        zip (takeWhile (before dx x2) [x1, x1 + dx ..])
                (takeWhile (before dy y2) [y1, y1 + dy ..]) ++
                        [(x2, y2)]

before :: (Num a, Ord a) => a -> a -> a -> Bool
before d t x = signum d * t >= signum d * x

getDirections :: Double -> Double -> [Double]
getDirections ds de = takeWhile beforeDir [ds, ds + dd ..] ++ [de]
        where
        sig = signum (de - ds)
        dd = sig * stepDir
        beforeDir x = sig * x < sig * de
