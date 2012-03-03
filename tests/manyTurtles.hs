import Graphics.X11.Turtle

main :: IO ()
main = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	red <- newTurtle f
	shape red "turtle"
	shapesize red 2 2
	pencolor red "red"
	penup red
	goto red (- 150) 100
	miniGreen <- newTurtle f
	shape miniGreen "turtle"
	pencolor miniGreen "green"
	penup miniGreen
	goto miniGreen (- 100) 50
	left miniGreen 90
	debuBrown <- newTurtle f
	shape debuBrown "turtle"
	shapesize debuBrown 1 3
	pencolor debuBrown "brown"
	penup debuBrown
	goto debuBrown (- 125) 0
	waitField f
