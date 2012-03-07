import Graphics.X11.Turtle.Field
import System.Environment

main = do
	[fn] <- getArgs
	f <- openField
	onkeypress f $ return . (/= 'q')
	l <- addLayer f
	drawImage f l fn (- 250) 180 500 360
	waitField f
