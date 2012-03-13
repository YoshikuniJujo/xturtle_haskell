module Horse(
	start,
	put,
	horse,
	flipV,
	flipH,
	invertColor,
	scale,
	superimpose,
	above,
	sidebyside
) where

import Graphics.X11.Turtle
import Control.Monad
import System.IO.Unsafe
import Data.IORef

type Picture = [[Bool]]

readDots :: FilePath -> IO Picture
readDots fn = do
	txt <- readFile fn
	return $ map toDots $ lines txt

toDots :: String -> [Bool]
toDots "" = []
toDots ('.' : ds) = False : toDots ds
toDots ('#' : ds) = True : toDots ds

flipH = reverse
flipV = map reverse
invertColor = map $ map not
scaleH n = concatMap (replicate n)
scale = flip scale_
scale_ n = concatMap (replicate n . concatMap (replicate n))
superimpose = zipWith (zipWith (||))
above = (++)
sidebyside = zipWith (++)

size = 4

horse = unsafePerformIO $ readDots "tests/horse.txt"

turtle :: IORef Turtle
turtle = unsafePerformIO $ newIORef undefined

field :: IORef Field
field = unsafePerformIO $ newIORef undefined

start :: IO ()
start = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	flushoff t
	hideturtle t
	penup t
	goto t (- 100) 100
	writeIORef field f
	writeIORef turtle t
	return ()

put :: Picture -> IO ()
put dots = do
	t <- readIORef turtle
	clear t
	forM_ dots $ \ln -> do
		forM_ ln $ (>> forward t size) . flip when (dot t size)
		backward t $ size * (fromIntegral $ length ln)
		right t 90 >> forward t size >> left t 90
	left t 90
	forward t $ size * (fromIntegral $ length dots)
	right t 90
	flush t

main = do
	start
	put $ sidebyside horse $ invertColor $ flipH horse
	readIORef field >>= waitField
