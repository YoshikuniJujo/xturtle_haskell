import Graphics.X11.Turtle
import Control.Monad

main = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	speed t "fastest"
	flushoff t
	shape t "turtle"
	forM_ [0, 10 .. 255] $ \i -> do
		moveBall t i
		moveTurtle t i
	waitField f

moveBall t i = do
	pencolor t (i, 0, 0)
	hideturtle t
	penup t
	goto t (- 200) 100
	pendown t
	replicateM_ 30 $ do
		penup t
		forward t 10
		pendown t
		ball t

moveTurtle t i = do
	pencolor t (0, i, 0)
	penup t
	goto t (- 200) 0
	shapesize t 2 2
	showturtle t
	flush t
	replicateM_ 30 $ do
		forward t 10
		sleep t 50
		flush t
	
-- moveBall :: Turtle -> Double -> Double -> IO ()

ball :: Turtle -> IO ()
ball t = do
	beginfill t
	circle t 20
	endfill t
	flush t
	sleep t 50
	replicateM_ 4 $ undo t
