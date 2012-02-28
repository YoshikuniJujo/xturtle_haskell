import Graphics.X11.Turtle

main :: IO ()
main = do
	f <- openField
	t <- newTurtle f
	hideturtle t
	forward t 100
	left t 90
	forward t 100
	waitField f
