module TurtleState (
) where

import WindowLayers
import SquareState
import Control.Monad
import Control.Concurrent

main :: IO ()
main = do
	f <- openField
	threadDelay 500000
	c <- addCharacter f
	forM_ movement $ \(x, y) -> do
		drawTurtle f c turtle 3 90 (x, y) $ Just (30, 30)
		threadDelay 100000
	forM_ rotation $ \d -> do
		drawTurtle f c turtle 3 d (100, 100) $ Just (30, 30)
		threadDelay 100000
	getLine >> return ()
--	closeField f

rotation :: [Double]
rotation = [90, 95 .. 180]

movement :: [(Double, Double)]
movement =
	[(30, 30), (40, 40), (50, 50), (60, 60), (70, 70), (80, 80), (90, 90), (100, 100)]

drawTurtle :: Field -> Character -> [(Double, Double)] -> Double -> Double ->
	(Double, Double) -> Maybe (Double, Double) -> IO ()
drawTurtle w c sh s d (x, y) org = do
	let sp = mkShape sh s d x y
	maybe (setPolygonCharacter w c sp)
		(flip (setPolygonCharacterAndLine w c sp) (x, y)) org
	bufToWin w
	flushWin w
