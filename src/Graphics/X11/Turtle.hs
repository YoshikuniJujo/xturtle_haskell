{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.X11.Turtle (
	-- * meta data
	xturtleVersion,

	-- * types and classes
	Field,
	Turtle,
	ColorClass,

	-- * beginings and endings
	openField,
	closeField,
	waitField,
	newTurtle,
	killTurtle,

	-- * move turtle
	forward,
	backward,
	goto,
	setx,
	sety,
	right,
	left,
	setheading,
	circle,
	write,
	home,
	clear,
	undo,

	-- * change turtle state
	shape,
	shapesize,
	showturtle,
	hideturtle,
	pendown,
	penup,
	pencolor,
	bgcolor,
	pensize,
	degrees,
	radians,

	-- * turtle information
	position,
	xcor,
	ycor,
	heading,
	towards,
	distance,
	isdown,
	isvisible,
	windowWidth,
	windowHeight,

	-- * on events
	onclick,
	onrelease,
	ondrag,
	onkeypress,

	-- * save and load
	getInputs,
	sendInputs,
	getSVG
) where

import Graphics.X11.Turtle.Move(
	Field, Layer, Character,
	forkField, openField, closeField,
	addCharacter, addLayer, fieldSize, clearLayer, clearCharacter,
	fieldColor, onclick, onrelease, ondrag, onkeypress, waitField,
	moveTurtle
 )
import Graphics.X11.Turtle.Input(
	TurtleInput(..), TurtleState,
	getTurtleStates, undonum, visible, direction,
	drawed
 )
import qualified Graphics.X11.Turtle.Input as S(degrees, pendown, position)
import Graphics.X11.Turtle.Shape(nameToShape)
import Text.XML.YJSVG(SVG(..), Color(..))
import Control.Concurrent(Chan, writeChan, threadDelay, ThreadId, killThread)
import Control.Monad(replicateM_, zipWithM_)
import Prelude hiding(Left)
import Data.IORef(IORef, newIORef, readIORef, modifyIORef)
import Data.Fixed(mod')

xturtleVersion :: (Int, String)
xturtleVersion = (37, "0.0.16c")

data Turtle = Turtle {
	field :: Field,
	layer :: Layer,
	character :: Character,
	inputChan :: Chan TurtleInput,
	states :: [TurtleState],
	inputs :: [TurtleInput],
	stateIndex :: IORef Int,
	thread :: ThreadId
 }

newTurtle :: Field -> IO Turtle
newTurtle f = do
	ch <- addCharacter f
	l <- addLayer f
	(ic, tis, sts) <- getTurtleStates
	si <- newIORef 1
	let	t = Turtle {
			field = f,
			inputChan = ic,
			layer = l,
			character = ch,
			states = sts,
			inputs = tis,
			stateIndex = si,
			thread = undefined
		 }
	tid <- forkField f $ zipWithM_ (moveTurtle f ch l) sts $ tail sts
	shape t "classic"
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
shape t = sendCommand t . Shape . nameToShape

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

pencolor :: ColorClass c => Turtle -> c -> IO ()
pencolor t c = sendCommand t $ Pencolor $ getColor c

class ColorClass a where
	getColor :: a -> Color

instance ColorClass String where
	getColor = ColorName

instance (Integral r, Integral g, Integral b) => ColorClass (r, g, b) where
	getColor (r, g, b) = RGB (fromIntegral r) (fromIntegral g) (fromIntegral b)

bgcolor :: ColorClass c => Field -> c -> IO ()
bgcolor f = fieldColor f . getColor

pensize :: Turtle -> Double -> IO ()
pensize t = sendCommand t . Pensize

degrees :: Turtle -> Double -> IO ()
degrees t = sendCommand t . Degrees

radians :: Turtle -> IO ()
radians = flip degrees $ 2 * pi

write :: Turtle -> String -> Double -> String -> IO ()
write t fnt sz = sendCommand t . Write fnt sz

undo :: Turtle -> IO ()
undo t = readIORef (stateIndex t)
	>>= flip replicateM_ (sendCommand t Undo) . undonum . (states t !!)

windowWidth, windowHeight :: Turtle -> IO Double
windowWidth = fmap fst . fieldSize . field
windowHeight = fmap snd . fieldSize . field

position :: Turtle -> IO (Double, Double)
position Turtle{stateIndex = si, states = s} =
	fmap (S.position . (s !!)) $ readIORef si

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
isdown t = fmap (S.pendown . (states t !!)) $ readIORef $ stateIndex t

isvisible :: Turtle -> IO Bool
isvisible t = fmap (visible . (states t !!)) $ readIORef $ stateIndex t

getInputs :: Turtle -> IO [TurtleInput]
getInputs t = do
	i <- readIORef $ stateIndex t
	return $ take (i - 1) $ inputs t

sendInputs :: Turtle -> [TurtleInput] -> IO ()
sendInputs t = mapM_ (sendCommand t)

getSVG :: Turtle -> IO [SVG]
getSVG t = fmap (reverse . drawed . (states t !!)) $ readIORef $ stateIndex t
