module TurtleDraw (
	Field,
	Layer,
	Character,

	openField,
	addLayer,
	addCharacter,
	turtleDraw
) where

import TurtleState
import DrawTurtle
import Control.Concurrent
import Control.Monad

turtleDraw :: Field -> Character -> Layer -> TurtleState -> TurtleState -> IO ()
turtleDraw f c l t0 t1 = do
	let	shape = turtleShape t1
		size = turtleSize t1
		prePos@(px, py) = turtlePos t0
		preDir = turtleDir t0
		pos@(nx, ny) = turtlePos t1
		dir = turtleDir t1
	forM_ (getPoints px py nx ny) $ \p -> do
		drawTurtle f c shape size dir p $ Just prePos
		threadDelay 50000
	line f l px py nx ny

step :: Double
step = 10

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
