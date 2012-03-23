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
import Data.Fixed(mod')

--------------------------------------------------------------------------------

xturtleVersion :: (Int, String)
xturtleVersion = (63, "0.1.8b")

--------------------------------------------------------------------------------

data Turtle = Turtle {
	field :: Field,
	states :: [TurtleState],
	index :: IORef Int,
	turtleInput :: Chan TurtleInput,
	turtleInputs :: [TurtleInput],
	turtleShapes :: IORef [(String, [(Double, Double)])],
	turtleThread :: ThreadId,
	turtleLayer :: Layer,
	turtleCharacter :: Character
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
	(chan, inpts, stts) <- turtleSeries
	idx <- newIORef 1
	thrd <- forkField f $ zipWithM_ (moveTurtle f char layer) stts $ tail stts
	shps <- newIORef shapeTable
	let	t = Turtle {
			field = f,
			states = stts,
			index = idx,
			turtleInput = chan,
			turtleInputs = inpts,
			turtleShapes = shps,
			turtleThread = thrd,
			turtleLayer = layer,
			turtleCharacter = char}
	shape t "classic" >> input t (Undonum 0)
	return t

killTurtle :: Turtle -> IO ()
killTurtle t = flushField (field t) True $ do
	clearLayer $ turtleLayer t
	clearCharacter $ turtleCharacter t
	killThread $ turtleThread t

inputs :: Turtle -> IO [TurtleInput]
inputs t = (flip take (turtleInputs t) . pred) `fmap` readIORef (index t)

runInputs :: Turtle -> [TurtleInput] -> IO ()
runInputs = mapM_ . input

getSVG :: Turtle -> IO [SVG]
getSVG = fmap reverse . information drawed

input :: Turtle -> TurtleInput -> IO ()
input t ti = atomicModifyIORef_ (index t) succ >> writeChan (turtleInput t) ti

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
setx t x = do
	pos <- position' t
	input t $ Goto $ case pos of
		Center _ y -> Center x y
		TopLeft _ y -> TopLeft x y
sety t y = do
	pos <- position' t
	input t $ Goto $ case pos of
		Center x _ -> Center x y
		TopLeft x _ -> TopLeft x y

left, right, setheading :: Turtle -> Double -> IO ()
left t = input t . TurnLeft
right t = left t . negate
setheading t = input t . Rotate

circle :: Turtle -> Double -> IO ()
circle t r = do
	deg <- information S.degrees t
	forward t (r * pi / 36)
	left t (deg / 36)
	replicateM_ 35 $ forward t (2 * r * pi / 36) >> left t (deg / 36)
	forward t (r * pi / 36)
	input t $ Undonum 74

home :: Turtle -> IO ()
home t = goto t 0 0 >> setheading t 0 >> input t (Undonum 3)

undo :: Turtle -> IO ()
undo t = information undonum t >>= flip replicateM_ (input t Undo)

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
image t fp = curry $ input t . uncurry (PutImage fp)

bgcolor :: ColorClass c => Turtle -> c -> IO ()
bgcolor t = input t . Bgcolor . getColor

clear :: Turtle -> IO ()
clear = (`input` Clear)

--------------------------------------------------------------------------------

addshape :: Turtle -> String -> [(Double, Double)] -> IO ()
addshape t n s = atomicModifyIORef_ (turtleShapes t) ((n, s) :)

beginpoly :: Turtle -> IO ()
beginpoly = (`input` SetPoly True)

endpoly :: Turtle -> IO [(Double, Double)]
endpoly t@Turtle{field = f} =
	input t (SetPoly False) >> information polyPoints t >>= mapM pos
	where pos p = do
		(w, h) <- windowSize t
		coord <- coordinates f
		return $ case coord of
			CoordCenter ->
				let Center x y = S.center w h p in (x, y)
			CoordTopLeft ->
				let TopLeft x y = S.topleft w h p in (x, y)

getshapes :: Turtle -> IO [String]
getshapes = fmap (map fst) . readIORef . turtleShapes

shape :: Turtle -> String -> IO ()
shape t n = readIORef (turtleShapes t) >>=
	maybe (putStrLn $ "no shape named " ++ n) (input t . Shape) . lookup n

shapesize :: Turtle -> Double -> Double -> IO ()
shapesize t = curry $ input t . uncurry Shapesize

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
	Just (ps, ds) -> do
		input t $ PositionStep ps
		input t $ DirectionStep ds
		input t $ Undonum 3
	Nothing -> putStrLn "no such speed"

flushoff, flushon :: Turtle -> IO ()
flushoff = (`input` SetFlush False)
flushon = (`input` SetFlush True)

--------------------------------------------------------------------------------

information :: (TurtleState -> a) -> Turtle -> IO a
information name Turtle{index = si, states = s} =
	fmap (name . (s !!)) $ readIORef si

position :: Turtle -> IO (Double, Double)
position t = do
	pos <- position' t
	return $ case pos of Center x y -> (x, y); TopLeft x y -> (x, y)

position' :: Turtle -> IO Position
position' t@Turtle{field = f} = do
	(w, h) <- windowSize t
	pos <- information S.position t
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
heading t = do
	deg <- information S.degrees t
	dir <- fmap (* (deg / (2 * pi))) $ information direction t
	return $ dir `mod'` deg

towards :: Turtle -> Double -> Double -> IO Double
towards t x0 y0 = do
	Center x y <- position' t
	deg <- information S.degrees t
	let	dir = atan2 (y0 - y) (x0 - x) * deg / (2 * pi)
	return $ if dir < 0 then dir + deg else dir

isdown, isvisible :: Turtle -> IO Bool
isdown = information S.pendown
isvisible = information visible

windowWidth, windowHeight :: Turtle -> IO Double
windowWidth = fmap fst . fieldSize . field
windowHeight = fmap snd . fieldSize . field

windowSize :: Turtle -> IO (Double, Double)
windowSize = fieldSize . field
