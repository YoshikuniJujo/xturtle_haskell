module Pictures(
	Picture,
	horse,
	white,
	printPicture,
	flipV,
	flipH,
	above,
	sideBySide,
	superimpose,
	invertColor,

	size,
	scale,
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

flipV, flipH :: Picture -> Picture
flipH = reverse
flipV = map reverse
invertColor :: Picture -> Picture
invertColor = map $ map not
scaleH n = concatMap (replicate n)
scale = flip scale_
scale_ n = concatMap (replicate n . concatMap (replicate n))
superimpose :: Picture -> Picture -> Picture
superimpose = zipWith (zipWith (||))
above, sideBySide :: Picture -> Picture -> Picture
above = (++)
sideBySide = zipWith (++)

size = writeIORef sizeRef

sizeRef = unsafePerformIO $ newIORef 1

horse, white :: Picture
horse = unsafePerformIO $ readDots "tests/horse.txt"
white = replicate 12 $ replicate 12 False

printPicture :: Picture -> IO ()
printPicture p = do
	s <- readIORef started
	unless s $ start
	put p

started :: IORef Bool
started = unsafePerformIO $ newIORef False

turtle :: IORef Turtle
turtle = unsafePerformIO $ newIORef undefined

field :: IORef Field
field = unsafePerformIO $ newIORef undefined

start :: IO ()
start = do
	f <- openField
	onkeypress f $ \c -> do
		writeIORef started False
		return $ c /= 'q'
	t <- newTurtle f
	flushoff t
	hideturtle t
	penup t
	goto t (- 100) 100
	writeIORef field f
	writeIORef turtle t
	writeIORef started True
	return ()

put :: Picture -> IO ()
put dots = do
	t <- readIORef turtle
	s <- readIORef sizeRef
	clear t
	forM_ dots $ \ln -> do
		forM_ ln $ (>> forward t s) . flip when (dot t s)
		backward t $ s * (fromIntegral $ length ln)
		right t 90 >> forward t s >> left t 90
	left t 90
	forward t $ s * (fromIntegral $ length dots)
	right t 90
	flush t

main = do
	start
	size 4
	put $ sideBySide horse $ invertColor $ flipH horse
	readIORef field >>= waitField
