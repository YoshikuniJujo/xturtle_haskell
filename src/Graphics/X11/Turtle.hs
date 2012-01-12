module Graphics.X11.Turtle (
	initTurtle,
	shapesize,
	goto,
	forward,
	backward,
	left,
	right,
	penup,
	pendown,
	isdown,
	home,
	circle,
	clear,
	undo,
	position,
	distance,
	getHistory,
) where

import qualified Graphics.X11.TurtleBase as Base
import Data.IORef
import Data.List
import System.IO.Unsafe
import Control.Monad
import Control.Concurrent
import Data.Maybe

world :: IORef Base.Turtle
world = unsafePerformIO $ newIORef undefined

initTurtle :: IO ()
initTurtle = Base.initTurtle >>= writeIORef world

shapesize :: Double -> IO ()
shapesize s = readIORef world >>= flip Base.shapesize s

windowWidth :: IO Double
windowWidth = readIORef world >>= Base.windowWidth

windowHeight :: IO Double
windowHeight = readIORef world >>= Base.windowHeight

position :: IO (Double, Double)
position = readIORef world >>= Base.position

distance :: Double -> Double -> IO Double
distance x0 y0 = do
	(x, y) <- position
	return $ ((x - x0) ** 2 + (y - y0) ** 2) ** (1 / 2)

penup :: IO ()
penup = readIORef world >>= Base.penup >> pushTurtleEvent Penup

pendown :: IO ()
pendown = readIORef world >>= Base.pendown >> pushTurtleEvent Pendown

isdown :: IO Bool
isdown = readIORef world >>= Base.isdown

goto, goto' :: Double -> Double -> IO ()
rawGoto :: Base.Turtle -> Double -> Double -> IO ()
goto x y = goto' x y >> setUndoPoint >> pushTurtleEvent (Goto x y)
goto' x y = do
	width <- windowWidth
	height <- windowHeight
	t <- readIORef world
	rawGoto t (x + width / 2) (- y + height / 2)

rawGoto t xTo yTo = do
	(act, past) <- Base.rawGotoGen t xTo yTo
	act
	dir <- Base.getDirection t
	forM_ past $ \(pos, act') -> putToPastDrawLines pos dir act'

forward, rawForward :: Double -> IO ()
forward len = rawForward len >> setUndoPoint >> pushTurtleEvent (Forward len)
rawForward len = do
	t <- readIORef world
	(x0, y0) <- Base.getPosition t
	d <- Base.getDirection t
	let	rad = d * pi / 180
		nx' = x0 + len * cos rad
		ny' = y0 + len * sin rad
	rawGoto t nx' ny'

backward :: Double -> IO ()
backward = forward . negate

rotateBy :: Double -> IO ()
rotateBy dd = do
	t <- readIORef world
	nd <- Base.rotateBy t dd
	pos <- Base.getPosition t
	putToPastDrawLines pos nd $ const (return ())

rotateTo :: Double -> IO ()
rotateTo d = do
	t <- readIORef world
	d0 <- Base.getDirection t
	let	step = 5
		dd = d - d0
	replicateM_ (abs dd `gDiv` step) $
		rotateBy (signum dd * step) >> threadDelay 10000
	Base.setDirection t d
	Base.flushW t

rotate, rawRotate :: Double -> IO ()
rotate d = rawRotate d >> setUndoPoint >> pushTurtleEvent (Rotate d)
rawRotate d = do
	d0 <- readIORef world >>= Base.getDirection
	rotateTo $ d0 + d

gDiv :: (Num a, Ord a, Integral b) => a -> a -> b
x `gDiv` y
	| x >= y = 1 + (x - y) `gDiv` y
	| otherwise = 0

right :: Double -> IO ()
right = rotate

left :: Double -> IO ()
left = rotate . negate

circle :: Double -> IO ()
circle r = replicateM_ 36 $ do
	rawForward (2 * r * pi / 36 :: Double)
	rawRotate (- 10)

home :: IO ()
home = goto' 0 0 >> rotateTo 0 >> pushTurtleEvent Home

clear :: IO ()
clear = do
	t <- readIORef world
	(retAct, (pos, dir, pastAct)) <- Base.clear t
	retAct
	putToPastDrawLines pos dir pastAct
	pushTurtleEvent Clear

--------------------------------------------------------------------------------

undo :: IO ()
undo = do
	t <- readIORef world
	dls <- fmap init $ readIORef pastDrawLines
	let	draw = map catMaybes
			$ takeWhile (isJust . last) $ reverse $ inits dls
		draw1 = map fromJust $ filter isJust $ head
			$ dropWhile (isJust . last) $ reverse $ inits dls
		draw' = draw ++ [draw1]
	forM_ (zip (reverse $ map (fst . fromJust) $ filter isJust dls )
		$ map (mapM_ (($ Base.BG) . snd)) draw') $ \((pos, dir), dl) -> do
		Base.initUndo t
		dl
		uncurry (Base.setPosition t) pos
		Base.setDirection t dir
		Base.flushW t
		threadDelay 20000
	modifyIORef pastDrawLines $ reverse . dropWhile isJust . tail . reverse
	pushTurtleEvent Undo

data TurtleEvent = Forward Double | Rotate Double | Undo | Home | Clear
	| Penup | Pendown | Goto Double Double
	deriving Show

getHistory :: IO [TurtleEvent]
getHistory = liftM (`take` turtleEvents) $ readIORef eventPoint 

eventChan :: Chan TurtleEvent
eventChan = unsafePerformIO newChan

turtleEvents :: [TurtleEvent]
turtleEvents = unsafePerformIO getTurtleEvents

getTurtleEvents :: IO [TurtleEvent]
getTurtleEvents = unsafeInterleaveIO $ do
	ev <- readChan eventChan
	evs <- getTurtleEvents
	return $ ev : evs

eventPoint :: IORef Int
eventPoint = unsafePerformIO $ newIORef 0

pushTurtleEvent :: TurtleEvent -> IO ()
pushTurtleEvent te = do
	writeChan eventChan te
	modifyIORef eventPoint (+ 1)

pastDrawLines :: IORef [Maybe (((Double, Double), Double), Base.Buf -> IO ())]
pastDrawLines = unsafePerformIO $ newIORef []

putToPastDrawLines :: (Double, Double) -> Double -> (Base.Buf -> IO ()) -> IO ()
putToPastDrawLines tpos tdir dl = do
	pdls <- readIORef pastDrawLines
	if length pdls < 300
		then writeIORef pastDrawLines $ pdls ++ [Just ((tpos, tdir), dl)]
		else do
			maybe (return ()) (($ Base.UndoBuf) . snd) $ head pdls
			writeIORef pastDrawLines $ tail pdls ++ [Just ((tpos, tdir), dl)]

setUndoPoint :: IO ()
setUndoPoint = modifyIORef pastDrawLines (++ [Nothing])
