module Graphics.X11.Turtle (
	Field,
	Turtle,

	openField,
	closeField,
	newTurtle,
	killTurtle,

	forward,
	backward,
	right,
	left,
	goto,
	setx,
	sety,
	setheading,
	home,
	circle,
	undo,

	position,
	xcor,
	ycor,
	heading,
	towards,
	distance,

	pendown,
	penup,
	isdown,

	bgcolor,
	pencolor,
	pensize,

	clear,

	showturtle,
	hideturtle,
	isvisible,

	shape,
	shapesize,

	degrees,
	radians,

	windowWidth,
	windowHeight,
	onclick,
	waitField,

	xturtleVersion
) where

import Graphics.X11.TurtleMove(
	Field, Layer, Character,
	forkIOX, openField, closeField,
	addCharacter, addLayer, layerSize, clearLayer, clearCharacter,
	addThread, fieldColor, onclick, waitField,
	moveTurtle
 )
import Graphics.X11.TurtleInput(
	TurtleInput(..), TurtleState,
	getTurtleStates, getPosition, getPendown, undonum, visible, direction
 )
import qualified Graphics.X11.TurtleInput as S(degrees)
import Graphics.X11.TurtleShape(lookupShape)
import Control.Concurrent(Chan, writeChan, threadDelay, ThreadId, killThread)
import Control.Monad(replicateM_, zipWithM_)
import Prelude hiding(Left)
import Data.IORef(IORef, newIORef, readIORef, modifyIORef)
import Data.Bits(shift, (.|.))
import Data.Word(Word8)
import Data.Fixed(mod')

xturtleVersion :: (Int, String)
xturtleVersion = (19, "0.0.11a")

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
	(ic, sts) <- getTurtleStates $ lookupShape "classic"
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

hideturtle, showturtle :: Turtle -> IO ()
hideturtle t = sendCommand t $ SetVisible False
showturtle t = sendCommand t $ SetVisible True

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

left, right, setheading :: Turtle -> Double -> IO ()
left t = sendCommand t . Left
right t = left t . negate
setheading t = sendCommand t . Rotate

goto :: Turtle -> Double -> Double -> IO ()
goto t x y = sendCommand t $ Goto x y

setx, sety :: Turtle -> Double -> IO ()
setx t x = do
	(_, y) <- position t
	sendCommand t $ Goto x y
sety t y = do
	(x, _) <- position t
	sendCommand t $ Goto x y

home :: Turtle -> IO ()
home t = goto t 0 0 >> sendCommand t (Rotate 0)

clear :: Turtle -> IO ()
clear t = sendCommand t Clear

circle :: Turtle -> Double -> IO ()
circle t r = do
	deg <- getDegrees t
	forward t (r * pi / 36)
	left t (deg / 36)
	replicateM_ 35 $ forward t (2 * r * pi / 36) >> left t (deg / 36)
	forward t (r * pi / 36)
	sendCommand t $ Undonum 74

getDegrees :: Turtle -> IO Double
getDegrees Turtle{stateIndex = si, states = s} =
	fmap (S.degrees . (s !!)) $ readIORef si

penup, pendown :: Turtle -> IO ()
penup = flip sendCommand Penup
pendown = flip sendCommand Pendown

pencolor :: Turtle -> Word8 -> Word8 -> Word8 -> IO ()
pencolor t r_ g_ b_ = sendCommand t $ Pencolor c
	where
	c = shift r 16 .|. shift g 8 .|. b
	[r, g, b] = map fromIntegral [r_, g_, b_]

bgcolor :: Field -> Word8 -> Word8 -> Word8 -> IO ()
bgcolor f r_ g_ b_ = fieldColor f c
	where
	c = shift r 16 .|. shift g 8 .|. b
	[r, g, b] = map fromIntegral [r_, g_, b_]

pensize :: Turtle -> Double -> IO ()
pensize t = sendCommand t . Pensize

degrees :: Turtle -> Double -> IO ()
degrees t = sendCommand t . Degrees

radians :: Turtle -> IO ()
radians = flip degrees $ 2 * pi

undo :: Turtle -> IO ()
undo t = readIORef (stateIndex t)
	>>= flip replicateM_ (sendCommand t Undo) . undonum . (states t !!)

windowWidth, windowHeight :: Turtle -> IO Double
windowWidth = fmap fst . layerSize . layer
windowHeight = fmap snd . layerSize . layer

position :: Turtle -> IO (Double, Double)
position Turtle{stateIndex = si, states = s} =
	fmap (getPosition . (s !!)) $ readIORef si

xcor, ycor :: Turtle -> IO Double
xcor = fmap fst . position
ycor = fmap snd . position

heading :: Turtle -> IO Double
heading t = do
	deg <- getDegrees t
	dir <- fmap (direction . (states t !!)) $ readIORef $ stateIndex t
	return $ dir `mod'` deg

towards :: Turtle -> Double -> Double -> IO Double
towards t x0 y0 = do
	(x, y) <- position t
	deg <- getDegrees t
	let	dir = atan2 (y0 - y) (x0 - x) * deg / (2 * pi)
	return $ if dir < 0 then dir + deg else dir

distance :: Turtle -> Double -> Double -> IO Double
distance t x0 y0 = do
	(x, y) <- position t
	return $ ((x - x0) ** 2 + (y - y0) ** 2) ** (1 / 2)

isdown :: Turtle -> IO Bool
isdown t = fmap (getPendown . (states t !!)) $ readIORef $ stateIndex t

isvisible :: Turtle -> IO Bool
isvisible t = fmap (visible . (states t !!)) $ readIORef $ stateIndex t
