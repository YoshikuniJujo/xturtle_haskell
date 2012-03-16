import Graphics.X11.Turtle
import Data.Word

main = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	onmotion f $ \x y -> do
		w <- windowWidth t
		h <- windowHeight t
		let	r = 255 * (x + w / 2) / w
			g = 255 * (y + h / 2) / h
			b = 255 - (r / 2) - (g / 2)
		bgcolor t (round r, round g, round b)
	waitField f

check :: Double -> Double
check n	| n > 255 = 255
	| n < 0 = 0
	| otherwise = n
