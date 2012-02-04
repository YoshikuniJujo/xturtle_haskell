module Graphics.X11.Turtle (
	Turtle,

	openField,
	closeField,
	newTurtle,
	killTurtle,

	shape,
	shapesize,
	hideturtle,
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

import Graphics.X11.TurtleMove(
	Field, Layer, Character,
	forkIOX, openField, closeField,
	addCharacter, addLayer, layerSize, clearLayer, clearCharacter,
	addThread,
	moveTurtle
 )
import Graphics.X11.TurtleInput(
	TurtleInput(..), TurtleState,
	getTurtleStates, getPosition, getPendown, undonum
 )
import Graphics.X11.TurtleShape(lookupShape, classic)
import Control.Concurrent(Chan, writeChan, threadDelay, ThreadId, killThread)
import Control.Monad(replicateM_, zipWithM_)
import Prelude hiding(Left)
import Data.IORef(IORef, newIORef, readIORef, modifyIORef)

xturtleVersion :: (Int, String)
xturtleVersion = (16, "0.0.9")

data Turtle = Turtle {
	layer :: Layer,
	character :: Character,
	inputChan :: Chan TurtleInput,
	states :: [TurtleState],
	stateIndex :: IORef Int,
	thread :: ThreadId
 }

newTurtle :: Field -> IO Turtle
newTurtle f = do
	ch <- addCharacter f
	l <- addLayer f
	(ic, sts) <- getTurtleStates classic
	si <- newIORef 1
	let	t = Turtle {
			inputChan = ic,
			layer = l,
			character = ch,
			states = sts,
			stateIndex = si,
			thread = undefined
		 }
	tid <- forkIOX $ zipWithM_ (moveTurtle ch l) sts $ tail sts
	addThread f tid
	return t{thread = tid}

killTurtle :: Turtle -> IO ()
killTurtle t = do
	clearLayer $ layer t
	clearCharacter $ character t
	killThread $ thread t

hideturtle :: Turtle -> IO ()
hideturtle t = sendCommand t $ SetVisible False

sendCommand :: Turtle -> TurtleInput -> IO ()
sendCommand Turtle{inputChan = c, stateIndex = si} ti = do
	modifyIORef si (+ 1)
	writeChan c ti
	threadDelay 10000

shape :: Turtle -> String -> IO ()
shape t = sendCommand t . Shape . lookupShape

shapesize :: Turtle -> Double -> IO ()
shapesize t = sendCommand t . ShapeSize

forward, backward :: Turtle -> Double -> IO ()
forward t = sendCommand t . Forward
backward t = forward t . negate

left, right :: Turtle -> Double -> IO ()
left t = sendCommand t . Left
right t = left t . negate

goto :: Turtle -> Double -> Double -> IO ()
goto t x y = sendCommand t $ Goto x y

home :: Turtle -> IO ()
home t = goto t 0 0 >> sendCommand t (Rotate 0)

clear :: Turtle -> IO ()
clear t = sendCommand t Clear

circle :: Turtle -> Double -> IO ()
circle t r = do
	forward t (r * pi / 36)
	left t 10
	replicateM_ 35 $ forward t (2 * r * pi / 36) >> left t 10
	forward t (r * pi / 36)
	sendCommand t $ Undonum 74

penup, pendown :: Turtle -> IO ()
penup = flip sendCommand Penup
pendown = flip sendCommand Pendown

undo :: Turtle -> IO ()
undo t = readIORef (stateIndex t)
	>>= flip replicateM_ (sendCommand t Undo) . undonum . (states t !!)

windowWidth, windowHeight :: Turtle -> IO Double
windowWidth = fmap fst . layerSize . layer
windowHeight = fmap snd . layerSize . layer

position :: Turtle -> IO (Double, Double)
position Turtle{stateIndex = si, states = s} =
	fmap (getPosition . (s !!)) $ readIORef si

distance :: Turtle -> Double -> Double -> IO Double
distance t x0 y0 = do
	(x, y) <- position t
	return $ ((x - x0) ** 2 + (y - y0) ** 2) ** (1 / 2)

isdown :: Turtle -> IO Bool
isdown t = fmap (getPendown . (states t !!)) $ readIORef $ stateIndex t
