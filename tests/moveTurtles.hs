import Graphics.X11.Turtle

main = do
	f <- openField
	t1 <- newTurtle f
	shape t1 "turtle"
	pencolor t1 "blue"
	shapesize t1 6 6
	penup t1
	speed t1 "fastest"
	t2 <- newTurtle f
	shape t2 "turtle"
	pencolor t2 "red"
	shapesize t2 3 3
	penup t2
	speed t2 "fastest"
	t3 <- newTurtle f
	shape t3 "turtle"
	shapesize t3 5 5
	penup t3
	speed t3 "fastest"
	onmotion f $ \x y -> do
		goto t1 x y
		left t1 10
		goto t2 (- x) (- y)
		right t2 10
		goto t3 y x
	onkeypress f $ return . (/= 'q')
	waitField f
