{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.X11.Turtle(
	-- * meta data
	xturtleVersion,

	-- * types and classes
	Field,
	Turtle,
	ColorClass,

	-- * Field functions
	-- ** meta
	openField,
	closeField,
	waitField,
	topleft,
	center,

	-- ** on events
	onclick,
	onrelease,
	ondrag,
	onmotion,
	onkeypress,
	ontimer,

	-- * Turtle functions
	-- ** meta
	newTurtle,
	killTurtle,
	inputs,
	runInputs,
	getSVG,

	-- ** move turtle
	forward,
	backward,
	goto,
	setx,
	sety,
	left,
	right,
	setheading,
	circle,
	home,
	undo,
	sleep,
	flush,

	-- ** draw
	dot,
	stamp,
	beginfill,
	endfill,
	write,
	image,
	bgcolor,
	clear,

	-- ** change states
	addshape,
	beginpoly,
	endpoly,
	getshapes,
	shape,
	shapesize,
	hideturtle,
	showturtle,
	penup,
	pendown,
	pencolor,
	pensize,
	radians,
	degrees,
	speed,
	flushoff,
	flushon,

	-- ** informations
	position,
	xcor,
	ycor,
	distance,
	heading,
	towards,
	isdown,
	isvisible,
	windowWidth,
	windowHeight
) where

import Graphics.X11.Turtle.Data(shapeTable, speedTable)
import Graphics.X11.Turtle.Input(
	TurtleState, TurtleInput(..),
	turtleSeries, direction, visible, undonum, drawed, polyPoints)
import qualified Graphics.X11.Turtle.Input as S(position, degrees, pendown)
import Graphics.X11.Turtle.Move(
	Field, Layer, Character, Coordinates(..),
	openField, closeField, fieldSize, coordinates, topleft, center,
	waitField, forkField, flushField, addLayer, clearLayer, addCharacter,
	clearCharacter, moveTurtle,
	onclick, onrelease, ondrag, onmotion, onkeypress, ontimer)
import Text.XML.YJSVG(SVG(..), Position(..), Color(..))
import qualified Text.XML.YJSVG as S(center, topleft)

import Control.Concurrent(ThreadId, Chan, killThread, writeChan)
import Control.Monad(replicateM_, zipWithM_)
import Data.IORef(IORef, newIORef, readIORef)
import Data.IORef.Tools(atomicModifyIORef_)
import Data.Maybe(fromJust)
import Data.Fixed(mod')

--------------------------------------------------------------------------------

xturtleVersion :: (Int, String)
xturtleVersion = (63, "0.1.8b")

--------------------------------------------------------------------------------

data Turtle = Turtle {
	field :: Field,
	states :: [TurtleState],
	index :: IORef Int,
	turtleLayer :: Layer,
	turtleCharacter :: Character,
	turtleInput :: Chan TurtleInput,
	turtleInputs :: [TurtleInput],
	turtleThread :: ThreadId,
	turtleShapes :: IORef [(String, [(Double, Double)])]
 }

class ColorClass a where getColor :: a -> Color

instance ColorClass String where getColor = ColorName

instance (Integral r, Integral g, Integral b) => ColorClass (r, g, b) where
	getColor (r, g, b) =
		RGB (fromIntegral r) (fromIntegral g) (fromIntegral b)

--------------------------------------------------------------------------------

newTurtle :: Field -> IO Turtle
newTurtle f = do
	layer <- addLayer f
	char <- addCharacter f
	(ic, tis, sts) <- turtleSeries
	si <- newIORef 1
	tid <- forkField f $ zipWithM_ (moveTurtle f char layer) sts $ tail sts
	shps <- newIORef shapeTable
	let	t = Turtle {
			field = f,
			turtleLayer = layer,
			turtleCharacter = char,
			turtleInput = ic,
			states = sts,
			turtleInputs = tis,
			index = si,
			turtleThread = tid,
			turtleShapes = shps}
	shape t "classic" >> input t (Undonum 0)
	return t

killTurtle :: Turtle -> IO ()
killTurtle t = flushField (field t) True $ do
	clearLayer $ turtleLayer t
	clearCharacter $ turtleCharacter t
	killThread $ turtleThread t

inputs, getInputs :: Turtle -> IO [TurtleInput]
inputs = getInputs
getInputs t = do
	i <- readIORef $ index t
	return $ take (i - 1) $ turtleInputs t

runInputs, sendInputs :: Turtle -> [TurtleInput] -> IO ()
runInputs = sendInputs
sendInputs t = mapM_ (input t)

getSVG :: Turtle -> IO [SVG]
getSVG t = fmap (reverse . drawed . (states t !!)) $ readIORef $ index t

input :: Turtle -> TurtleInput -> IO ()
input Turtle{turtleInput = c, index = si} ti =
	atomicModifyIORef_ si (+ 1) >>writeChan c ti

--------------------------------------------------------------------------------

forward, backward :: Turtle -> Double -> IO ()
forward t = input t . Forward
backward t = forward t . negate

goto :: Turtle -> Double -> Double -> IO ()
goto t@Turtle{field = f} x y = do
	coord <- coordinates f
	input t $ Goto $ case coord of
		CoordCenter -> Center x y
		CoordTopLeft -> TopLeft x y

setx, sety :: Turtle -> Double -> IO ()
setx t@Turtle{field = f} x = do
	(w, h, pos) <- posAndSize t
	coord <- coordinates f
	input t $ Goto $ case coord of
		CoordCenter -> let Center _ y = S.center w h pos in Center x y
		CoordTopLeft -> let TopLeft _ y = S.topleft w h pos in TopLeft x y
sety t@Turtle{field = f} y = do
	(w, h, pos) <- posAndSize t
	coord <- coordinates f
	input t $ Goto $ case coord of
		CoordCenter -> let Center x _ = S.center w h pos in Center x y
		CoordTopLeft -> let TopLeft x _ = S.topleft w h pos in TopLeft x y

posAndSize :: Turtle -> IO (Double, Double, Position)
posAndSize t = do
	(w, h) <- windowSize t
	pos <- position' t
	return (w, h, pos)

left, right, setheading :: Turtle -> Double -> IO ()
left t@Turtle{field = f} d = do
	coord <- coordinates f
	input t $ TurnLeft $ case coord of CoordCenter -> d; CoordTopLeft -> d
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

home :: Turtle -> IO ()
home t = goto t 0 0 >> setheading t 0 >> input t (Undonum 3)

undo :: Turtle -> IO ()
undo t = readIORef (index t)
	>>= flip replicateM_ (input t Undo) . undonum . (states t !!)

sleep :: Turtle -> Int -> IO ()
sleep t = input t . Sleep

flush :: Turtle -> IO ()
flush = (`input` Flush)

--------------------------------------------------------------------------------

dot :: Turtle -> Double -> IO ()
dot t = input t . Dot

stamp :: Turtle -> IO ()
stamp = (`input` Stamp)

beginfill, endfill :: Turtle -> IO ()
beginfill = (`input` SetFill True)
endfill = (`input` SetFill False)

write :: Turtle -> String -> Double -> String -> IO ()
write t fnt sz = input t . Write fnt sz

image :: Turtle -> FilePath -> Double -> Double -> IO ()
image t fp w h = input t $ PutImage fp w h

bgcolor :: ColorClass c => Turtle -> c -> IO ()
bgcolor t = input t . Bgcolor . getColor

clear :: Turtle -> IO ()
clear t = input t Clear

--------------------------------------------------------------------------------

addshape :: Turtle -> String -> [(Double, Double)] -> IO ()
addshape t n s = atomicModifyIORef_ (turtleShapes t) ((n, s) :)

beginpoly :: Turtle -> IO ()
beginpoly = (`input` SetPoly True)

endpoly :: Turtle -> IO [(Double, Double)]
endpoly t@Turtle{index = si, states = s} = do
	input t $ SetPoly False
	fmap (polyPoints . (s !!)) (readIORef si) >>= mapM (getPos t)

getPos :: Turtle -> Position -> IO (Double, Double)
getPos t@Turtle{field = f} pos = do
	w <- windowWidth t
	h <- windowHeight t
	coord <- coordinates f
	return $ case coord of
		CoordCenter -> let Center x y = S.center w h pos in (x, y)
		CoordTopLeft -> let TopLeft x y = S.topleft w h pos in (x, y)

getshapes :: Turtle -> IO [String]
getshapes t = fmap (map fst) $ readIORef (turtleShapes t)

shape :: Turtle -> String -> IO ()
shape t n = readIORef (turtleShapes t) >>= input t . Shape . fromJust . lookup n

shapesize :: Turtle -> Double -> Double -> IO ()
shapesize t sx sy = input t $ Shapesize sx sy

hideturtle, showturtle :: Turtle -> IO ()
hideturtle = (`input` SetVisible False)
showturtle = (`input` SetVisible True)

penup, pendown :: Turtle -> IO ()
penup = (`input` SetPendown False)
pendown = (`input` SetPendown True)

pencolor :: ColorClass c => Turtle -> c -> IO ()
pencolor t = input t . Pencolor . getColor

pensize :: Turtle -> Double -> IO ()
pensize t = input t . Pensize

radians :: Turtle -> IO ()
radians = (`degrees` (2 * pi))

degrees :: Turtle -> Double -> IO ()
degrees t = input t . Degrees

speed :: Turtle -> String -> IO ()
speed t str = case lookup str speedTable of
	Just (ps, ds) -> input t (PositionStep ps) >> input t (DirectionStep ds)
	Nothing -> putStrLn "no such speed"

flushoff, flushon :: Turtle -> IO ()
flushoff = (`input` SetFlush False)
flushon = (`input` SetFlush True)

--------------------------------------------------------------------------------

position :: Turtle -> IO (Double, Double)
position t = do
	pos <- position' t
	return $ case pos of
		Center x y -> (x, y)
		TopLeft x y -> (x, y)

position' :: Turtle -> IO Position
position' t@Turtle{field = f, index = si, states = s} = do
	w <- windowWidth t
	h <- windowHeight t
	pos <- fmap (S.position . (s !!)) $ readIORef si
	coord <- coordinates f
	return $ case coord of
		CoordCenter -> S.center w h pos
		CoordTopLeft -> S.topleft w h pos

xcor, ycor :: Turtle -> IO Double
xcor = fmap fst . position
ycor = fmap snd . position

distance :: Turtle -> Double -> Double -> IO Double
distance t x0 y0 = do
	Center x y <- position' t
	return $ ((x - x0) ** 2 + (y - y0) ** 2) ** (1 / 2)

heading :: Turtle -> IO Double
heading t@Turtle{index = si, states = s} = do
	deg <- getDegrees t
	dir <- fmap ((* (deg / (2 * pi))) . direction . (s !!)) $ readIORef si
	return $ dir `mod'` deg

getDegrees :: Turtle -> IO Double
getDegrees Turtle{index = si, states = s} =
	fmap (S.degrees . (s !!)) $ readIORef si

towards :: Turtle -> Double -> Double -> IO Double
towards t x0 y0 = do
	Center x y <- position' t
	deg <- getDegrees t
	let	dir = atan2 (y0 - y) (x0 - x) * deg / (2 * pi)
	return $ if dir < 0 then dir + deg else dir

isdown, isvisible :: Turtle -> IO Bool
isdown t = fmap (S.pendown . (states t !!)) $ readIORef $ index t
isvisible t = fmap (visible . (states t !!)) $ readIORef $ index t

windowWidth, windowHeight :: Turtle -> IO Double
windowWidth = fmap fst . fieldSize . field
windowHeight = fmap snd . fieldSize . field

windowSize :: Turtle -> IO (Double, Double)
windowSize = fieldSize . field
