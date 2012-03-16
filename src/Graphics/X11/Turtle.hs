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
	stamp,
	dot,
	image,
	bgcolor,
	home,
	clear,
	undo,
	sleep,
	flushoff,
	flushon,
	flush,

	-- * change turtle state
	addshape,
	getshapes,
	shape,
	shapesize,
	speed,
	hideturtle,
	showturtle,
	penup,
	pendown,
	beginfill,
	endfill,
	beginpoly,
	endpoly,
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
	onmotion,
	onkeypress,
	ontimer,

	-- * save and load
	getInputs,
	sendInputs,
	getSVG,

	-- * others
	topleft,
	center
) where

import Graphics.X11.Turtle.Data(shapeTable, speedTable)
import Graphics.X11.Turtle.Input(
	TurtleState, TurtleInput(..),
	turtleSeries, direction, visible, undonum, drawed, polyPoints)
import qualified Graphics.X11.Turtle.Input as S(position, degrees, pendown)
import Graphics.X11.Turtle.Move(
	Field(coordinates), Layer, Character, Coordinates(..),
	openField, closeField, forkField, waitField, fieldSize, flushField,
	moveTurtle, addLayer, clearLayer, addCharacter, clearCharacter,
	onclick, onrelease, ondrag, onmotion, onkeypress, ontimer)
import Text.XML.YJSVG(Position(..), SVG(..), Color(..))
import qualified Text.XML.YJSVG as S (center, topleft)

import Control.Concurrent(Chan, writeChan, ThreadId, killThread)
import Control.Monad(replicateM_, zipWithM_)
import Data.IORef(IORef, newIORef, readIORef, modifyIORef, writeIORef)
import Data.IORef.Tools(atomicModifyIORef_)
import Data.Fixed(mod')
import Data.Maybe(fromJust)

--------------------------------------------------------------------------------

xturtleVersion :: (Int, String)
xturtleVersion = (61, "0.1.8")

--------------------------------------------------------------------------------

data Turtle = Turtle {
	field :: Field,
	layer :: Layer,
	character :: Character,
	inputChan :: Chan TurtleInput,
	states :: [TurtleState],
	inputs :: [TurtleInput],
	stateIndex :: IORef Int,
	thread :: ThreadId,
	shapes :: IORef [(String, [(Double, Double)])]
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
	shps <- newIORef shapeTable
	let	t = Turtle {
			field = f,
			layer = l,
			character = ch,
			inputChan = ic,
			states = sts,
			inputs = tis,
			stateIndex = si,
			thread = tid,
			shapes = shps}
	shape t "classic" >> input t (Undonum 0)
	return t

killTurtle :: Turtle -> IO ()
killTurtle t = flushField (field t) True $ do
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
goto t@Turtle{field = f} x y = do
	coord <- readIORef $ coordinates f
	input t $ Goto $ case coord of
		C -> Center x y
		TL -> TopLeft x y

setx, sety :: Turtle -> Double -> IO ()
setx t@Turtle{field = f} x = do
	w <- windowWidth t
	h <- windowHeight t
	pos <- position' t
	coord <- readIORef $ coordinates f
	input t $ Goto $ case coord of
		C -> let Center _ y = S.center w h pos in Center x y
		TL -> let TopLeft _ y = S.topleft w h pos in TopLeft x y
sety t@Turtle{field = f} y = do
	w <- windowWidth t
	h <- windowHeight t
	pos <- position' t
	coord <- readIORef $ coordinates f
	input t $ Goto $ case coord of
		C -> let Center x _ = S.center w h pos in Center x y
		TL -> let TopLeft x _ = S.topleft w h pos in TopLeft x y

left, right, setheading :: Turtle -> Double -> IO ()
left t@Turtle{field = f} d = do
	coord <- readIORef $ coordinates f
	input t $ TurnLeft $ case coord of C -> d; TL -> d
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

stamp :: Turtle -> IO ()
stamp = (`input` Stamp)

dot :: Turtle -> Double -> IO ()
dot t = input t . Dot

image :: Turtle -> FilePath -> Double -> Double -> IO ()
image t fp w h = input t $ PutImage fp w h

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

flushoff, flushon :: Turtle -> IO ()
flushoff = (`input` SetFlush False)
flushon = (`input` SetFlush True)

flush :: Turtle -> IO ()
flush = (`input` Flush)

--------------------------------------------------------------------------------

addshape :: Turtle -> String -> [(Double, Double)] -> IO ()
addshape t n s = modifyIORef (shapes t) ((n, s) :)

getshapes :: Turtle -> IO [String]
getshapes t = fmap (map fst) $ readIORef (shapes t)

shape :: Turtle -> String -> IO ()
shape t n = readIORef (shapes t) >>= input t . Shape . fromJust . lookup n

shapesize :: Turtle -> Double -> Double -> IO ()
shapesize t sx sy = input t $ Shapesize sx sy

speed :: Turtle -> String -> IO ()
speed t str = case lookup str speedTable of
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

beginpoly :: Turtle -> IO ()
beginpoly = (`input` SetPoly True)

endpoly :: Turtle -> IO [(Double, Double)]
endpoly t@Turtle{stateIndex = si, states = s} = do
	input t $ SetPoly False
	fmap (polyPoints . (s !!)) (readIORef si) >>= mapM (getPos t)

getPos :: Turtle -> Position -> IO (Double, Double)
getPos t@Turtle{field = f} pos = do
	w <- windowWidth t
	h <- windowHeight t
	coord <- readIORef $ coordinates f
	return $ case coord of
		C -> let Center x y = S.center w h pos in (x, y)
		TL -> let TopLeft x y = S.topleft w h pos in (x, y)

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
position t@Turtle{field = f, stateIndex = si, states = s} = do
	w <- windowWidth t
	h <- windowHeight t
	pos <- fmap (S.position . (s !!)) $ readIORef si
	coord <- readIORef $ coordinates f
	case coord of
		C -> let Center x y = S.center w h pos in return (x, y)
		TL -> let TopLeft x y = S.topleft w h pos in return (x, y)

position' :: Turtle -> IO Position
position' t@Turtle{field = f, stateIndex = si, states = s} = do
	w <- windowWidth t
	h <- windowHeight t
	pos <- fmap (S.position . (s !!)) $ readIORef si
	coord <- readIORef $ coordinates f
	return $ case coord of
		C -> S.center w h pos
		TL -> S.topleft w h pos

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
	Center x y <- position' t
	deg <- getDegrees t
	let	dir = atan2 (y0 - y) (x0 - x) * deg / (2 * pi)
	return $ if dir < 0 then dir + deg else dir

distance :: Turtle -> Double -> Double -> IO Double
distance t x0 y0 = do
	Center x y <- position' t
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

--------------------------------------------------------------------------------

topleft ::  Field -> IO ()
topleft f = writeIORef (coordinates f) TL

center :: Field -> IO ()
center f = writeIORef (coordinates f) C
