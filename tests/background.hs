import Graphics.X11.Turtle
import Control.Monad
import Control.Concurrent
import Data.Word

main = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	hideturtle t
	penup t
	forM_ [0, 1 .. 255] $ \i -> setcolor t (i, 0, 0) -- bgcolor t (i, 0, 0)
	forM_ [0, 1 .. 255] $ \i -> setcolor t (255 - i, i, 0) -- bgcolor t (255 - i, i, 0)
	forM_ [0, 1 .. 255] $ \i -> bgcolor t (0, 255 - i, i)
	forM_ [0, 1 .. 255] $ \i -> bgcolor t (i, 0, 255 - i)
	forM_ [0, 1 .. 255] $ \i -> bgcolor t (255 - i, i `div` 2, i `div` 2)
	forM_ [0, 1 .. 255] $ \i -> bgcolor t (i `div` 2, 128 - i `div` 2, 128 + i `div` 2)
	forM_ [0, 1 .. 255] $ \i -> bgcolor t (128 + i `div` 2, i, 255)
	forM_ [0, 1 .. 255] $ \i -> bgcolor t (255 - i, 255 - i, 255 - i)
	waitField f

setcolor :: Turtle -> (Word8, Word8, Word8) -> IO ()
setcolor t c = do
--	w <- windowWidth t
--	h <- windowHeight t
--	print (w, h)
--	goto t (- w / 2) (h / 2 - 14)
--	write t "KochiMincho" 12 $ show c
--	print c
	bgcolor t c
