import Graphics.X11.Turtle
import Control.Monad

main :: IO ()
main = do
	f <- openField
	t <- newTurtle f
	shape t "turtle"
	replicateM_ 4 $ do
		forward t 100
		left t 90
	replicateM_ 4 $ do
		backward t 100
		left t 90
	silentundo t 4
	left t 90
	replicateM_ 4 $ do
		backward t 100
		left t 90
	onkeypress f $ return . ('q' /=)
	waitField f
