module Graphics.X11.Turtle (
	Turtle,

	openField,
	newTurtle,

	shape,
	shapesize,
	forward,
	backward,
	left,
	right,
	goto,
	home,
	clear,
	circle,
	penup,
	pendown,
	undo,

	windowWidth,
	windowHeight,
	position,
	distance,
	isdown,

	xturtleVersion
) where

import Graphics.X11.TurtleDraw
import Graphics.X11.TurtleInput
import Control.Concurrent
import Control.Monad
import Prelude hiding(Left)
import Data.IORef
import Control.Arrow(second)

xturtleVersion :: (Int, String)
xturtleVersion = (1, "0.0.7")

data Turtle = Turtle {
	inputChan :: Chan TurtleInput,
	states :: [TurtleState],
	stateNow :: IORef Int,
	layer :: Layer,
	character :: Character
 }

newTurtle :: Field -> IO Turtle
newTurtle f = do
	ch <- addCharacter f
	l <- addLayer f
	(c, sts) <- getTurtleStates classic
	sn <- newIORef 1
	let	t = Turtle {
			inputChan = c,
			layer = l,
			character = ch,
			states = sts,
			stateNow = sn
		 }
	_ <- forkIOX $ for2M_ sts $ turtleDraw ch l
	return t

sendCommand :: Turtle -> TurtleInput -> IO ()
sendCommand Turtle{inputChan = c, stateNow = sn} ti = do
	modifyIORef sn (+ 1)
	writeChan c ti
	threadDelay 10000

shape :: Turtle -> String -> IO ()
shape t "turtle" = sendCommand t $ Shape turtle
shape t "classic" = sendCommand t $ Shape classic
shape _ name = error $ "There is no shape named " ++ name

shapesize :: Turtle -> Double -> IO ()
shapesize t = sendCommand t . ShapeSize

forward, backward :: Turtle -> Double -> IO ()
forward t = sendCommand t . Forward
backward t = forward t . negate

left, right :: Turtle -> Double -> IO ()
left t = sendCommand t . Left
right t = left t . negate

circle :: Turtle -> Double -> IO ()
circle t r = do
	forward t (r * pi / 36)
	left t 10
	replicateM_ 35 $ forward t (2 * r * pi / 36) >> left t 10
	forward t (r * pi / 36)
	sendCommand t $ Undonum 74

home :: Turtle -> IO ()
home t = goto t 0 0 >> rotate t 0

clear :: Turtle -> IO ()
clear t@Turtle{layer = l} = do
	left t 0
	clearLayer l

position :: Turtle -> IO (Double, Double)
position Turtle{stateNow = sn, states = s} =
	fmap (getPosition . (s !!)) $ readIORef sn

distance :: Turtle -> Double -> Double -> IO Double
distance t x0 y0 = do
	(x, y) <- position t
	return $ ((x - x0) ** 2 + (y - y0) ** 2) ** (1 / 2)

windowWidth, windowHeight :: Turtle -> IO Double
windowWidth = fmap fst . layerSize . layer
windowHeight = fmap snd . layerSize . layer

pendown, penup :: Turtle -> IO ()
pendown = flip sendCommand Pendown
penup = flip sendCommand Penup

isdown :: Turtle -> IO Bool
isdown Turtle{states = s, stateNow = sn} =
	fmap (getPendown . (s !!)) $ readIORef sn

goto :: Turtle -> Double -> Double -> IO ()
goto t x y = sendCommand t $ Goto x y

rotate :: Turtle -> Double -> IO ()
rotate t = sendCommand t . Rotate

undo :: Turtle -> IO ()
undo t = do
	un <- getUndoNum t
	replicateM_ un $ sendCommand t Undo

getUndoNum :: Turtle -> IO Int
getUndoNum Turtle{states = s, stateNow = sn} =
	fmap (undonum . (s!!)) $ readIORef sn

for2M_ :: [a] -> (a -> a -> IO b) -> IO ()
for2M_ xs f = zipWithM_ f xs $ tail xs

classic :: [(Double, Double)]
classic = clssc ++ reverse (map (second negate) clssc)
	where
	clssc = [
		(- 10, 0),
		(- 16, 6),
		(0, 0)
	 ]

turtle :: [(Double, Double)]
turtle = ttl ++ reverse (map (second negate) ttl)
	where
	ttl = [
		(- 10, 0),
		(- 8, - 3),
		(- 10, - 5),
		(- 7, - 9),
		(- 5, - 6),
		(0, - 8),
		(4, - 7),
		(6, - 10),
		(8, - 7),
		(7, - 5),
		(10, - 2),
		(13, - 3),
		(16, 0)
	 ]
