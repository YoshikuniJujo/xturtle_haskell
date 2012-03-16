import Graphics.X11.Turtle
import Control.Monad
import Control.Concurrent
import Text.XML.YJSVG hiding (topleft)
import System.Environment

main = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	hideturtle t
	penup t
	flushoff t
	threadDelay 1000000
	w <- windowWidth t
	h <- windowHeight t
	topleft f
	home t
--	goto t (fromIntegral $ truncate $ - w / 2) (fromIntegral $ truncate $ h / 2)
	replicateM_ 100 $ do
		forM_ [0 .. 255] $ \r -> do
			pencolor t (r, 0, 0)
			dot t 1
			forward t 1
		right t 90 >> forward t 1 >> right t 90 >> forward t 256
		left t 180
	flush t
	replicateM_ 100 $ do
		forM_ [0 .. 255] $ \g -> do
			pencolor t (0, g, 0)
			dot t 1
			forward t 1
		right t 90 >> forward t 1 >> right t 90 >> forward t 256
		left t 180
	flush t
	replicateM_ 100 $ do
		forM_ [0 .. 255] $ \b -> do
			pencolor t (0, 0, b)
			dot t 1
			forward t 1
		right t 90 >> forward t 1 >> right t 90 >> forward t 256
		left t 180
	flush t
	waitField f
	args <- getArgs
	case args of
		[fn] -> do
			svg <- getSVG t
			writeFile fn $ showSVG w h svg
		_ -> return ()
