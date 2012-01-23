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
	writeChan c $ Goto 0 0
	forkIO $ do
--		initialThread
		for2M_ sts $ turtleDraw f ch l
	return t

shape :: Turtle -> String -> IO ()
shape Turtle{inputChan = c} "turtle" = writeChan c $ Shape turtle
shape Turtle{inputChan = c} "classic" = writeChan c $ Shape classic

shapesize :: Turtle -> Double -> IO ()
shapesize Turtle{inputChan = c} = writeChan c . ShapeSize

forward :: Turtle -> Double -> IO ()
forward Turtle{inputChan = c} = writeChan c . Forward

goto :: Turtle -> Double -> Double -> IO ()
goto Turtle{inputChan = c} x y = writeChan c $ Goto x y

rotateTo :: Turtle -> Double -> IO ()
rotateTo Turtle{inputChan = c} d = writeChan c $ RotateTo d

undo :: Turtle -> IO ()
undo Turtle{inputChan = c} = writeChan c Undo

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
	writeChan c $ Goto 0 0
	writeChan c $ RotateTo 0
	writeChan c $ RotateTo 180
	writeChan c $ Goto 50 100
	writeChan c $ Goto 100 50
	writeChan c $ Goto 100 100
	writeChan c $ RotateTo (- 180)
	writeChan c Undo
	writeChan c Undo
	writeChan c Undo
	print $ take 12 ret
	let turtles = inputToTurtle [] initialTurtleState ret
	print $ drop 4 $ take 12 turtles
	turtleDraw f ch l (turtles !! 4) (turtles !! 5)
	turtleDraw f ch l (turtles !! 5) (turtles !! 6)
	turtleDraw f ch l (turtles !! 6) (turtles !! 7)
	turtleDraw f ch l (turtles !! 7) (turtles !! 8)
	turtleDraw f ch l (turtles !! 8) (turtles !! 9)
	turtleDraw f ch l (turtles !! 9) (turtles !! 10)
	turtleDraw f ch l (turtles !! 10) (turtles !! 11)
	turtleDraw f ch l (turtles !! 11) (turtles !! 12)

testUndo :: IO ()
testUndo = do
	f <- openField
	t <- newTurtle f
	goto t 100 100
	goto t 100 200
	rotateTo t 180
	undo t
	undo t
	mapM_ print $ take 6 $ states t
