import Graphics.X11.Turtle
import Control.Arrow

horse = [
	(8, 10), (- 3, 5), (- 8, - 2), (2, - 10), (3, 0), (9, - 4)
 ]

flipV = map $ first negate
scale s = map ((* s) *** (* s))

main = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	addshape t "horse" $ scale 2 $ flipV horse
	shape t "horse"
	shapesize t 2 2
	waitField f
