import Turtle
import Control.Monad

spiral :: Double -> Double -> IO ()
spiral size angle
	| size > 100	= return ()
	| otherwise	= do
		forward size
		right angle
		spiral (size + 2) angle

qcircle :: IO ()
qcircle = replicateM_ 9 $ forward 10 >> right 10

leaf :: IO ()
leaf = qcircle >> right 90 >> qcircle

flower :: IO ()
flower = do
	left 90
	forward 50
	clear
	replicateM_ 9 $ leaf >> right 10
	right 180
	forward 200
	right 180
	forward 30
	right 20
	leaf

triangles :: Double -> IO ()
triangles size
	| size < 10	= return ()
	| otherwise	= do
		right 60
		replicateM_ 3 $ forward size >> right 120
		forward $ size / 2
		triangles $ size / 2

polygon :: Double -> Int -> IO ()
polygon size repeats =
	replicateM_ repeats $ forward size >> right (fromIntegral $ 360 `div` repeats)
