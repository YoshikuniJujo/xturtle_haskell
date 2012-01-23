module NewTurtle (
) where

import TurtleDraw
import TurtleInput
import Control.Concurrent
import Control.Monad

data Turtle = Turtle {
	inputChan :: Chan TurtleInput,
	field :: Field,
	layer :: Layer,
	character :: Character,
	states :: [TurtleState]
 }

newTurtle :: Field -> IO Turtle
newTurtle f = do
	ch <- addCharacter f
	l <- addLayer f
	(c, ret) <- makeInput
	let	sts = drop 4 $ inputToTurtle [] initialTurtleState ret
		t = Turtle {
			inputChan = c,
			field = f,
			layer = l,
			character = ch,
			states = sts
		 }
	writeChan c $ Shape classic
	writeChan c $ ShapeSize 1
	writeChan c $ PenDown
	writeChan c $ Goto 0 0
	writeChan c $ RotateTo 0
	forkIO $ do
--		initialThread
		for2M_ sts $ turtleDraw f ch l
	return t

goto :: Turtle -> Double -> Double -> IO ()
goto Turtle{inputChan = c} x y = writeChan c $ Goto x y

for2M_ :: [a] -> (a -> a -> IO b) -> IO ()
for2M_ xs f = zipWithM_ f xs $ tail xs

main :: IO ()
main = do
	putStrLn "module NewTurtle"

	f <- openField
	ch <- addCharacter f
	l <- addLayer f

	(c, ret) <- makeInput
	writeChan c $ Shape classic
	writeChan c $ ShapeSize 1
	writeChan c $ PenDown
	writeChan c $ Goto 30 30
	writeChan c $ RotateTo 0
	writeChan c $ Goto 100 100
	print $ take 6 ret
	let turtles = inputToTurtle [] initialTurtleState ret
	print $ drop 4 $ take 6 turtles
	turtleDraw f ch l (turtles !! 4) (turtles !! 5)

