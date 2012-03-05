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
	left,
	right,
	setheading,
	circle,
	write,
	bgcolor,
	home,
	clear,
	undo,
	sleep,

	-- * change turtle state
	shape,
	shapesize,
	speed,
	hideturtle,
	showturtle,
	penup,
	pendown,
	beginfill,
	endfill,
	pencolor,
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

import Graphics.X11.Turtle.Data(nameToShape, nameToSpeed)
import Graphics.X11.Turtle.Input(
	TurtleState, TurtleInput(..),
	turtleSeries, direction, visible, undonum, drawed)
import qualified Graphics.X11.Turtle.Input as S(position, degrees, pendown)
import Graphics.X11.Turtle.Move(
	Field, Layer, Character,
	openField, closeField, forkField, waitField, fieldSize, flushField,
	moveTurtle, addLayer, clearLayer, addCharacter, clearCharacter,
	onclick, onrelease, ondrag, onkeypress)
import Text.XML.YJSVG(SVG(..), Color(..))

import Control.Concurrent(Chan, writeChan, ThreadId, killThread)
import Control.Monad(replicateM_, zipWithM_)
import Data.IORef(IORef, newIORef, readIORef)
import Data.IORef.Tools(atomicModifyIORef_)
import Data.Fixed(mod')

--------------------------------------------------------------------------------

xturtleVersion :: (Int, String)
xturtleVersion = (46, "0.1.0")

--------------------------------------------------------------------------------

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

class ColorClass a where getColor :: a -> Color

instance ColorClass String where getColor = ColorName

instance (Integral r, Integral g, Integral b) => ColorClass (r, g, b) where
	getColor (r, g, b) =
		RGB (fromIntegral r) (fromIntegral g) (fromIntegral b)

--------------------------------------------------------------------------------

newTurtle :: Field -> IO Turtle
newTurtle f = do
	l <- addLayer f
	ch <- addCharacter f
	(ic, tis, sts) <- turtleSeries
	si <- newIORef 1
	tid <- forkField f $ zipWithM_ (moveTurtle f ch l) sts $ tail sts
	let	t = Turtle {
			field = f,
			layer = l,
			character = ch,
			inputChan = ic,
			states = sts,
			inputs = tis,
			stateIndex = si,
			thread = tid}
	shape t "classic" >> input t (Undonum 0)
	return t

killTurtle :: Turtle -> IO ()
killTurtle t = flushField (field t) $ do
	clearLayer $ layer t
	clearCharacter $ character t
	killThread $ thread t

input :: Turtle -> TurtleInput -> IO ()
input Turtle{inputChan = c, stateIndex = si} ti =
	atomicModifyIORef_ si (+ 1) >>writeChan c ti

--------------------------------------------------------------------------------

forward, backward :: Turtle -> Double -> IO ()
forward t = input t . Forward
backward t = forward t . negate

goto :: Turtle -> Double -> Double -> IO ()
goto t x y = input t $ Goto x y

setx, sety :: Turtle -> Double -> IO ()
setx t x = do
	(_, y) <- position t
	input t $ Goto x y
sety t y = do
	(x, _) <- position t
	input t $ Goto x y

left, right, setheading :: Turtle -> Double -> IO ()
left t = input t . TurnLeft
right t = left t . negate
setheading t = input t . Rotate

circle :: Turtle -> Double -> IO ()
circle t r = do
	deg <- getDegrees t
	forward t (r * pi / 36)
	left t (deg / 36)
	replicateM_ 35 $ forward t (2 * r * pi / 36) >> left t (deg / 36)
	forward t (r * pi / 36)
	input t $ Undonum 74

write :: Turtle -> String -> Double -> String -> IO ()
write t fnt sz = input t . Write fnt sz

bgcolor :: ColorClass c => Turtle -> c -> IO ()
bgcolor t = input t . Bgcolor . getColor

home :: Turtle -> IO ()
home t = goto t 0 0 >> setheading t 0 >> input t (Undonum 3)

clear :: Turtle -> IO ()
clear t = input t Clear

undo :: Turtle -> IO ()
undo t = readIORef (stateIndex t)
	>>= flip replicateM_ (input t Undo) . undonum . (states t !!)

sleep :: Turtle -> Int -> IO ()
sleep t = input t . Sleep

--------------------------------------------------------------------------------

shape :: Turtle -> String -> IO ()
shape t = input t . Shape . nameToShape

shapesize :: Turtle -> Double -> Double -> IO ()
shapesize t sx sy = input t $ Shapesize sx sy

speed :: Turtle -> String -> IO ()
speed t str = case nameToSpeed str of
	Just (ps, ds) -> input t (PositionStep ps) >> input t (DirectionStep ds)
	Nothing -> putStrLn "no such speed"

hideturtle, showturtle :: Turtle -> IO ()
hideturtle = (`input` SetVisible False)
showturtle = (`input` SetVisible True)

penup, pendown :: Turtle -> IO ()
penup = (`input` SetPendown False)
pendown = (`input` SetPendown True)

beginfill, endfill :: Turtle -> IO ()
beginfill = (`input` SetFill True)
endfill = (`input` SetFill False)

pencolor :: ColorClass c => Turtle -> c -> IO ()
pencolor t = input t . Pencolor . getColor

pensize :: Turtle -> Double -> IO ()
pensize t = input t . Pensize

degrees :: Turtle -> Double -> IO ()
degrees t = input t . Degrees

radians :: Turtle -> IO ()
radians = (`degrees` (2 * pi))

--------------------------------------------------------------------------------

position :: Turtle -> IO (Double, Double)
position Turtle{stateIndex = si, states = s} =
	fmap (S.position . (s !!)) $ readIORef si

xcor, ycor :: Turtle -> IO Double
xcor = fmap fst . position
ycor = fmap snd . position

heading :: Turtle -> IO Double
heading t@Turtle{stateIndex = si, states = s} = do
	deg <- getDegrees t
	dir <- fmap ((* (deg / (2 * pi))) . direction . (s !!)) $ readIORef si
	return $ dir `mod'` deg

getDegrees :: Turtle -> IO Double
getDegrees Turtle{stateIndex = si, states = s} =
	fmap (S.degrees . (s !!)) $ readIORef si

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

isdown, isvisible :: Turtle -> IO Bool
isdown t = fmap (S.pendown . (states t !!)) $ readIORef $ stateIndex t
isvisible t = fmap (visible . (states t !!)) $ readIORef $ stateIndex t

windowWidth, windowHeight :: Turtle -> IO Double
windowWidth = fmap fst . fieldSize . field
windowHeight = fmap snd . fieldSize . field

--------------------------------------------------------------------------------

getInputs :: Turtle -> IO [TurtleInput]
getInputs t = do
	i <- readIORef $ stateIndex t
	return $ take (i - 1) $ inputs t

sendInputs :: Turtle -> [TurtleInput] -> IO ()
sendInputs t = mapM_ (input t)

getSVG :: Turtle -> IO [SVG]
getSVG t = fmap (reverse . drawed . (states t !!)) $ readIORef $ stateIndex t
