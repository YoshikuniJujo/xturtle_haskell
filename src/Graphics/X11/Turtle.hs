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
	home,
	clear,
	circle,
	undo,
	position,
	distance,
	windowWidth,
	windowHeight,
	goto,
	penup,
	pendown,
	isdown
) where

import Graphics.X11.TurtleDraw
import Graphics.X11.TurtleInput
import Control.Concurrent
import Control.Monad
import Prelude hiding(Left)
import Data.IORef
import Control.Arrow(second)

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
shape Turtle{inputChan = c, stateNow = sn} "turtle" = do
	modifyIORef sn (+ 1)
	writeChan c $ Shape turtle
shape Turtle{inputChan = c, stateNow = sn} "classic" = do
	modifyIORef sn (+ 1)
	writeChan c $ Shape classic
shape _ name = error $ "There is no shape named " ++ name

shapesize :: Turtle -> Double -> IO ()
shapesize Turtle{inputChan = c, stateNow = sn} size = do
	modifyIORef sn (+ 1)
	writeChan c $ ShapeSize size

forward, backward :: Turtle -> Double -> IO ()
forward Turtle{inputChan = c, stateNow = sn} len = do
	modifyIORef sn (+1)
	writeChan c $ Forward len
	threadDelay 10000
backward t = forward t . negate

left, right :: Turtle -> Double -> IO ()
left Turtle{inputChan = c, stateNow = sn} dd = do
	modifyIORef sn (+ 1)
	writeChan c $ Left dd
	threadDelay 10000
right t = left t . negate

circle :: Turtle -> Double -> IO ()
circle t@Turtle{inputChan = c, stateNow = sn} r = do
	forward t (r * pi / 36)
	left t 10
	replicateM_ 35 $ forward t (2 * r * pi / 36) >> left t 10
	forward t (r * pi / 36)
	writeChan c $ SetUndoNum 74
	modifyIORef sn (+ 1)

home :: Turtle -> IO ()
home t = modifyIORef (stateNow t) (+ 1) >> goto t 0 0 >> rotateTo t 0

clear :: Turtle -> IO ()
clear t@Turtle{field = f, layer = l} = do
	forward t 0
	clearLayer f l

position :: Turtle -> IO (Double, Double)
position Turtle{stateNow = sn, states = s} =
	fmap (turtlePos . (s !!)) $ readIORef sn

distance :: Turtle -> Double -> Double -> IO Double
distance t x0 y0 = do
	(x, y) <- position t
	return $ ((x - x0) ** 2 + (y - y0) ** 2) ** (1 / 2)

windowWidth, windowHeight :: Turtle -> IO Double
windowWidth = fmap fst . winSize . field
windowHeight = fmap snd . winSize . field

pendown, penup :: Turtle -> IO ()
pendown Turtle{inputChan = c, stateNow = sn} = do
	modifyIORef sn (+ 1)
	writeChan c PenDown
penup Turtle{inputChan = c, stateNow = sn} = do
	modifyIORef sn (+ 1)
	writeChan c PenUp

isdown :: Turtle -> IO Bool
isdown Turtle{states = s, stateNow = sn} =
	fmap (turtlePenDown . (s !!)) $ readIORef sn

goto :: Turtle -> Double -> Double -> IO ()
goto Turtle{inputChan = c, stateNow = sn} x y = do
	modifyIORef sn (+ 1)
	writeChan c $ Goto x y

rotateTo :: Turtle -> Double -> IO ()
rotateTo Turtle{inputChan = c} d = writeChan c $ RotateTo d

undo :: Turtle -> IO ()
undo t@Turtle{inputChan = c, stateNow = sn} = do
	un <- getUndoNum t
	replicateM_ un $ do
		modifyIORef sn (+1)
		writeChan c Undo

getUndoNum :: Turtle -> IO Int
getUndoNum Turtle{states = s, stateNow = sn} =
	fmap (turtleUndoNum . (s!!)) $ readIORef sn

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
