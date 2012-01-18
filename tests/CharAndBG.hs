module CharAndBG where

import WindowLayers
import Control.Monad.Tools
import Control.Concurrent
import Data.IORef

data Square = Square{
	sLayer :: Layer,
	sChar :: Character,
	sPos :: IORef (Double, Double)
 }

main :: IO ()
main = do
	w <- openWin
	l0 <- addLayer w
	c0 <- addCharacter w
	c1 <- addCharacter w
	line w l0 20 20 200 200
	setPolygonCharacter w c0 [(10, 10), (10, 20), (20, 20), (20, 10)]
	setPolygonCharacter w c1 [(110, 10), (110, 20), (120, 20), (120, 10)]
	doWhile 0 $ \t -> do
		setPolygonCharacterAndLine w c0
			[(10, 10 + 10 * t), (10, 20 + 10 * t), (20, 20 + 10 * t),
				(20, 10 + 10 * t)]
			(15, 15) (15, 15 + 10 * t)
		threadDelay 200000
		return (t + 1, t < 10)
	line w l0 15 15 15 115
	doWhile 0 $ \t -> do
		setPolygonCharacterAndLine w c0
			[(10 + 10 * t, 110), (10 + 10 * t, 120), (20 + 10 * t, 120),
				(20 + 10 * t, 110)]
			(15, 115) (15 + 10 * t, 115)
		threadDelay 200000
		return (t + 1, t < 10)
	getLine >> return ()
{-
	setPolygonCharacter w c0 [(10, 110), (10, 120), (20, 120), (20, 110)]
	getLine >> return ()
-}

newSquare :: Win -> IO Square
newSquare w = do
	l <- addLayer w
	c <- addCharacter w
	p <- newIORef (0, 0)
	return $ Square{
		sLayer = l,
		sChar = c,
		sPos = p
	 }

step :: Double
step = 10

getPoints :: Double -> Double -> Double -> Double -> [(Double, Double)]
getPoints x1 y1 x2 y2 = let
	len = ((x2 - x1) ^ 2 + (y2 - y1) ^ 2) ** (1/2)
	dx = (x2 - x1) * 10 / len
	dy = (y2 - y1) * 10 / len in
	zip (takeWhile (before dx x2) [x1, x1 + dx ..])
		(takeWhile (before dy y2) [y1, y1 + dy ..]) ++
			[(x2, y2)]

before :: (Num a, Ord a) => a -> a -> a -> Bool
before d t x = signum d * t >= signum d * x

showAnimation :: Win -> Square -> Double -> Double -> IO ()
showAnimation w s@Square{sPos = p} x2 y2 = do
	(x1, y1) <- readIORef p
	setPolygonCharacterAndLine w (sChar s) 
		[(x2, y2), (x2 + 10, y2), (x2 + 10, y2 + 10), (x2, y2 + 10)]
		(x1, y1) (x2, y2)

moveSquare :: Win -> Square -> Double -> Double -> IO ()
moveSquare w s@Square{sPos = p} x2 y2 = do
	(x1, y1) <- readIORef p
	mapM_ (\(x, y) -> showAnimation w s x y >> threadDelay 50000) $
		getPoints x1 y1 x2 y2
	writeIORef p (x2, y2)
	line w (sLayer s) x1 y1 x2 y2
	setPolygonCharacter w (sChar s)
		[(x2, y2), (x2 + 10, y2), (x2 + 10, y2 + 10), (x2, y2 + 10)]
