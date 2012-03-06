import Graphics.X11.Turtle
import Control.Monad
import System.Random
import Control.Concurrent
import Text.XML.YJSVG

main = do
	g1 <- newStdGen
	g2 <- newStdGen
	let	xs = randomRs (- 250, 250) g1
		ys = randomRs (50, 150) g2
		smallStars = zip xs ys
	f <- openField
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	flushoff t
	hideturtle t
	bgcolor t "black"
	flush t
--	threadDelay 1000000
	house t (- 100) (- 150)
	house t (- 200) (- 150)
	uncurry (star t 1) `mapM_` take 30 smallStars
	flush t
	waitField f
	w <- windowWidth t
	h <- windowHeight t
	getSVG t >>= putStr . showSVG w h

smallStars = [
	(- 100, 100),
	(90, 90)
 ]

star t s x y = do
	penup t
	goto t x y
	pendown t
	pencolor t "yellow"
	beginfill t
	replicateM_ 5 $ forward t (s * 5) >> left t 144
	endfill t
	forward t $ s * 5 / (2 + 2 * sin18)
	beginfill t
	replicateM_ 5 $ forward t (s * 5 - s * 5 / (1 + sin18)) >> left t 72
	endfill t
--	flush t
	where
	sin18 = sin $ pi / 10

house t x y = do
	penup t
	goto t x y -- (- 100) (-150)
	pendown t
	setheading t 0
	pendown t
	pencolor t "darkred"
	beginfill t
	forward t 80 
	left t 150
	forward t 30
	left t 30
	forward t 30
	left t 30
	forward t 30
	endfill t
	setheading t 0
	forward t 10
	pencolor t "darkgrey"
	beginfill t
	setheading t 270
	forward t 30
	left t 90
	forward t 60
	left t 90
	forward t 30
	endfill t
	penup t
	left t 90
	forward t 45
	left t 90
	forward t 5
	pendown t
	pencolor t "yellow"
	beginfill t
	forward t 10
	left t 90
	forward t 10
	left t 90
	forward t 10
	endfill t
--	flush t
