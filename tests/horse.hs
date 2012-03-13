module Main where

import Graphics.X11.Turtle
import Control.Monad

t = True
f = False

dots :: [[Bool]]
dots = [
	[t, t, t, t, t, t, t, t, f, f, f, f, f, t, t, t, t, t],
	[t, t, t, t, t, t, t, t, f, f, f, f, f, t, t, t, t, t],
	[t, t, t, t, t, t, t, t, f, f, f, f, f, t, t, t, t, t],
	[t, t, t, t, t, t, t, t, f, f, f, f, f, t, t, t, t, t]
 ]

readDots :: FilePath -> IO [[Bool]]
readDots fn = do
	txt <- readFile fn
	return $ map toDots $ lines txt

toDots :: String -> [Bool]
toDots "" = []
toDots ('.' : ds) = False : toDots ds
toDots ('#' : ds) = True : toDots ds

flipV = reverse
flipH = map reverse
invertColor = map $ map not
scaleH n = concatMap (replicate n)
scale n = concatMap (replicate n . concatMap (replicate n))
superimpose = zipWith (zipWith (||))
above = (++)
sidebyside = zipWith (++)

size = 4

main = do
	horse <- readDots "tests/horse.txt"
	f <- openField
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	flushoff t
	hideturtle t
	penup t
	goto t (- 100) 100
	forM_ (sidebyside horse (flipH horse)) $ \ln -> do
		forM_ ln $ (>> forward t size) . flip when (dot t size)
		backward t $ size * (fromIntegral $ length ln)
		right t 90 >> forward t size >> left t 90
	flush t
	waitField f
