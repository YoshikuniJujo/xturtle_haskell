module Graphics.X11.Turtle (
	openField,
	newTurtle,
	Turtle,
	shape,
	shapesize,
	forward,
	backward,
	left,
	right,
	undo,
	position,
	distance,
	home,
	circle
) where

import Graphics.X11.TurtleDraw
import Graphics.X11.TurtleInput
import Control.Concurrent
import Control.Monad
import Prelude hiding (Left)
import Data.IORef

data Turtle = Turtle {
	inputChan :: Chan TurtleInput,
	field :: Field,
	layer :: Layer,
	character :: Character,
	states :: [TurtleState],
	stateNow :: IORef Int
 }

newTurtle :: Field -> IO Turtle
newTurtle f = do
	ch <- addCharacter f
	l <- addLayer f
	(c, ret) <- makeInput
	sn <- newIORef 1
	let	sts = drop 4 $ inputToTurtle [] initialTurtleState ret
		t = Turtle {
			inputChan = c,
			field = f,
			layer = l,
			character = ch,
			states = sts,
			stateNow = sn
		 }
	writeChan c $ Shape classic
	writeChan c $ ShapeSize 1
	writeChan c PenDown
	writeChan c $ Goto 0 0
	writeChan c $ RotateTo 0
	writeChan c $ Goto 0 0
	_ <- forkIO $
--		initialThread
		for2M_ sts $ turtleDraw f ch l
	return t

shape :: Turtle -> String -> IO ()
shape Turtle{inputChan = c} "turtle" = writeChan c $ Shape turtle
shape Turtle{inputChan = c} "classic" = writeChan c $ Shape classic
shape _ name = error $ "There is no shape named " ++ name

shapesize :: Turtle -> Double -> IO ()
shapesize Turtle{inputChan = c} = writeChan c . ShapeSize

forward, backward :: Turtle -> Double -> IO ()
forward Turtle{inputChan = c, stateNow = sn} len = do
	modifyIORef sn (+1)
	writeChan c $ Forward len
backward t = forward t . negate

left, right :: Turtle -> Double -> IO ()
left Turtle{inputChan = c, stateNow = sn} dd = do
	modifyIORef sn (+ 1)
	writeChan c $ Left dd
right t = left t . negate

circle :: Turtle -> Double -> IO ()
circle t r = do
	forward t (r * pi / 36)
	left t 10
	replicateM_ 35 $ forward t (2 * r * pi / 36) >> left t 10
	forward t (r * pi / 36)

home :: Turtle -> IO ()
home t = modifyIORef (stateNow t) (+ 1) >> goto t 0 0 >> rotateTo t 0

position :: Turtle -> IO (Double, Double)
position Turtle{stateNow = sn, states = s} =
	fmap (turtlePos . (s !!)) $ readIORef sn

distance :: Turtle -> Double -> Double -> IO Double
distance t x0 y0 = do
	(x, y) <- position t
	return $ ((x - x0) ** 2 + (y - y0) ** 2) ** (1 / 2)

goto :: Turtle -> Double -> Double -> IO ()
goto Turtle{inputChan = c} x y = writeChan c $ Goto x y

rotateTo :: Turtle -> Double -> IO ()
rotateTo Turtle{inputChan = c} d = writeChan c $ RotateTo d

undo :: Turtle -> IO ()
undo Turtle{inputChan = c, stateNow = sn} = do
	modifyIORef sn (+1)
	writeChan c Undo

for2M_ :: [a] -> (a -> a -> IO b) -> IO ()
for2M_ xs f = zipWithM_ f xs $ tail xs
