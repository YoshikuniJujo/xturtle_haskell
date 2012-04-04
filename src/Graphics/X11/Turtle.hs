{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}

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
	TurtleState, TurtleInput(..), inputToTurtleSeries)
import Graphics.X11.Turtle.State(
	direction, visible, undonum, drawed, polyPoints)
import qualified Graphics.X11.Turtle.State as S(position, degrees, pendown)
import Graphics.X11.Turtle.Move(
	Field, Coordinates(..), openField, closeField, fieldSize,
	coordinates, topleft, center, waitField, forkField, flushField,
	addLayer, clearLayer, addCharacter, clearCharacter, moveTurtle,
	onclick, onrelease, ondrag, onmotion, onkeypress, ontimer)
import Text.XML.YJSVG(SVG(..), Position(..), Color(..))
import qualified Text.XML.YJSVG as S(center, topleft)

import Control.Concurrent(killThread, newChan, writeChan, getChanContents)
import Control.Monad(replicateM_, zipWithM_)
import Control.Arrow((&&&))
import Data.IORef(IORef, newIORef, readIORef)
import Data.IORef.Tools(atomicModifyIORef_)
import Data.Fixed(mod')

--------------------------------------------------------------------------------

xturtleVersion :: (Int, String)
xturtleVersion = (67, "0.1.10")

--------------------------------------------------------------------------------

data Turtle = Turtle {
	field :: Field, input :: TurtleInput -> IO (),
	info :: forall a . (TurtleState -> a) -> IO a,
	shapes :: IORef [(String, [(Double, Double)])],
	inputs :: IO [TurtleInput], killTurtle :: IO ()}

class ColorClass a where getColor :: a -> Color

instance ColorClass String where getColor = ColorName

instance (Integral r, Integral g, Integral b) => ColorClass (r, g, b) where
	getColor (r, g, b) =
		RGB (fromIntegral r) (fromIntegral g) (fromIntegral b)

--------------------------------------------------------------------------------

newTurtle :: Field -> IO Turtle
newTurtle f = do
	index <- newIORef 1; shapesRef <- newIORef shapeTable
	chan <- newChan
	hist <- getChanContents chan
	let states = inputToTurtleSeries hist
	l <- addLayer f; c <- addCharacter f
	thr <- forkField f $ zipWithM_ (moveTurtle f c l) states $ tail states
	let t = Turtle {
		field = f,
		input = (atomicModifyIORef_ index succ >>) . writeChan chan,
		info = \n -> fmap (n . (states !!)) $ readIORef index,
		shapes = shapesRef,
		inputs = fmap (flip take hist . pred) $ readIORef index,
		killTurtle = flushField f True $
			clearLayer l >> clearCharacter c >> killThread thr}
	shape t "classic" >> input t (Undonum 0) >> return t

runInputs :: Turtle -> [TurtleInput] -> IO ()
runInputs = mapM_ . input

getSVG :: Turtle -> IO [SVG]
getSVG = fmap reverse . flip info drawed

convertPosition :: Turtle -> Position -> IO Position
convertPosition t p = do
	(w, h) <- windowSize t
	coord <- coordinates $ field t
	return $ case coord of
		CoordCenter -> S.center w h p
		CoordTopLeft -> S.topleft w h p

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
	pos <- info t S.position >>= convertPosition t
	input t $ Goto $ case pos of
		Center _ y -> Center x y
		TopLeft _ y -> TopLeft x y
sety t y = do
	pos <- info t S.position >>= convertPosition t
	input t $ Goto $ case pos of
		Center x _ -> Center x y
		TopLeft x _ -> TopLeft x y

left, right, setheading :: Turtle -> Double -> IO ()
left t = input t . TurnLeft
right t = left t . negate
setheading t = input t . Rotate

circle :: Turtle -> Double -> IO ()
circle t r = do
	deg <- info t S.degrees
	forward t (r * pi / 36)
	left t (deg / 36)
	replicateM_ 35 $ forward t (2 * r * pi / 36) >> left t (deg / 36)
	forward t (r * pi / 36)
	input t $ Undonum 74

home :: Turtle -> IO ()
home t = goto t 0 0 >> setheading t 0 >> input t (Undonum 3)

undo :: Turtle -> IO ()
undo t = info t undonum >>= flip replicateM_ (input t Undo)

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
addshape t n s = atomicModifyIORef_ (shapes t) ((n, s) :)

beginpoly :: Turtle -> IO ()
beginpoly = (`input` SetPoly True)

endpoly :: Turtle -> IO [(Double, Double)]
endpoly t = input t (SetPoly False) >> info t polyPoints >>=
	mapM (fmap (posX &&& posY) . convertPosition t)

getshapes :: Turtle -> IO [String]
getshapes = fmap (map fst) . readIORef . shapes

shape :: Turtle -> String -> IO ()
shape t n = readIORef (shapes t) >>=
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

position :: Turtle -> IO (Double, Double)
position t = fmap (posX &&& posY) $ info t S.position >>= convertPosition t

xcor, ycor :: Turtle -> IO Double
xcor = fmap fst . position
ycor = fmap snd . position

distance :: Turtle -> Double -> Double -> IO Double
distance t x0 y0 = do
	(x, y) <- position t
	return $ ((x - x0) ** 2 + (y - y0) ** 2) ** (1 / 2)

heading :: Turtle -> IO Double
heading t = do
	deg <- info t S.degrees
	dir <- fmap (* (deg / (2 * pi))) $ info t direction
	return $ dir `mod'` deg

towards :: Turtle -> Double -> Double -> IO Double
towards t x0 y0 = do
	(x, y) <- position t
	deg <- info t S.degrees
	let	dir = atan2 (y0 - y) (x0 - x) * deg / (2 * pi)
	return $ if dir < 0 then dir + deg else dir

isdown, isvisible :: Turtle -> IO Bool
isdown = flip info S.pendown
isvisible = flip info visible

windowWidth, windowHeight :: Turtle -> IO Double
windowWidth = fmap fst . fieldSize . field
windowHeight = fmap snd . fieldSize . field

windowSize :: Turtle -> IO (Double, Double)
windowSize = fieldSize . field
