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
	undoAll,
	undo,
	distance,
	getHistory
) where

import qualified Graphics.X11.TurtleBase as Base
import Data.IORef
import Data.List
import System.IO.Unsafe
import Control.Monad.Tools
import Control.Monad
import Control.Concurrent
import Data.Maybe

data TurtleEvent = Forward Double | Rotate Double | Undo | Home | Clear
	| Penup | Pendown | Goto Double Double
	deriving Show

data Buf = BG | UndoBuf

getHistory :: IO [TurtleEvent]
getHistory = readIORef eventPoint >>= return . flip take turtleEvents

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

world :: IORef Base.Turtle
world = unsafePerformIO $ newIORef undefined

windowWidth :: IO Double
windowWidth = readIORef world >>= fmap fst . Base.winSize . Base.wWin . Base.tWorld

windowHeight :: IO Double
windowHeight = readIORef world >>= fmap snd . Base.winSize . Base.wWin . Base.tWorld

position :: IO (Double, Double)
position = do
	w <- fmap Base.tWorld $ readIORef world
	(x, y) <- Base.getCursorPos w
	width <- windowWidth
	height <- windowHeight
	return (x - width / 2, height / 2 - y)

distance :: Double -> Double -> IO Double
distance x0 y0 = do
	(x, y) <- position
	return $ ((x - x0) ** 2 + (y - y0) ** 2) ** (1 / 2)

pastDrawLines :: IORef [Maybe (((Double, Double), Double), Buf -> IO ())]
pastDrawLines = unsafePerformIO $ newIORef []

putToPastDrawLines :: (Double, Double) -> Double -> (Buf -> IO ()) -> IO ()
putToPastDrawLines tpos tdir dl = do
	pdls <- readIORef pastDrawLines
	if length pdls < 300
		then do
			writeIORef pastDrawLines $ pdls ++ [Just ((tpos, tdir), dl)]
		else do
			maybe (return ()) (($ UndoBuf) . snd) $ head pdls
			writeIORef pastDrawLines $ tail pdls ++ [Just ((tpos, tdir), dl)]

setUndoPoint :: IO ()
setUndoPoint = modifyIORef pastDrawLines $ (++ [Nothing])

data PenState = PenUp | PenDown

doesPenDown :: PenState -> Bool
doesPenDown PenUp = False
doesPenDown PenDown = True

penState :: IORef PenState
penState = unsafePerformIO $ newIORef PenDown

initTurtle :: IO ()
initTurtle = Base.initTurtle >>= writeIORef world

shapesize :: Double -> IO ()
shapesize s = do
	w <- fmap Base.tWorld $ readIORef world
	Base.setCursorSize w s
	Base.drawWorld w
	Base.flushWorld $ Base.wWin w

goto, goto', rawGoto :: Double -> Double -> IO ()
goto x y = do
	width <- windowWidth
	height <- windowHeight
	rawGoto (x + width / 2) (- y + height / 2)
		>> setUndoPoint >> pushTurtleEvent (Goto x y)
goto' x y = do
	width <- windowWidth
	height <- windowHeight
	rawGoto (x + width / 2) (- y + height / 2)
rawGoto xTo yTo = do
	w <- fmap Base.tWorld $ readIORef world
	(x0, y0) <- Base.getCursorPos w
	let	step = 10
		distX = xTo - x0
		distY = yTo - y0
		dist = (distX ** 2 + distY ** 2) ** (1 / 2)
		dx = step * distX / dist
		dy = step * distY / dist
	(x', y') <- if (dist <= step) then return (x0, y0) else doWhile (x0, y0) $ \(x, y) -> do
		let	nx = x + dx
			ny = y + dy
		Base.setCursorPos w nx ny
		drawLine (Base.wWin w) x y nx ny
		Base.drawWorld w
		Base.flushWorld $ Base.wWin w
		threadDelay 20000
		return ((nx, ny),
			(nx + dx - x0) ** 2 + (ny + dy - y0) ** 2 < dist ** 2)
	Base.setCursorPos w xTo yTo
	drawLine (Base.wWin w) x' y' xTo yTo
	Base.drawWorld w
	Base.flushWorld $ Base.wWin w

forward, rawForward :: Double -> IO ()
forward len = rawForward len >> setUndoPoint >> pushTurtleEvent (Forward len)
rawForward len = do
	w <- fmap Base.tWorld $ readIORef world
	(x0, y0) <- Base.getCursorPos w
	d <- Base.getCursorDir w
	let	rad = d * pi / 180
		nx' = x0 + len * cos rad
		ny' = y0 + len * sin rad
	rawGoto nx' ny'

backward :: Double -> IO ()
backward = forward . negate

penup :: IO ()
penup = writeIORef penState PenUp >> pushTurtleEvent Penup

pendown :: IO ()
pendown = writeIORef penState PenDown >> pushTurtleEvent Pendown

isdown :: IO Bool
isdown = do
	ps <- readIORef penState
	return $ case ps of
		PenUp -> False
		PenDown -> True

drawLine :: Base.Win -> Double -> Double -> Double -> Double -> IO ()
drawLine w x1 y1 x2 y2 = do
	let act buf = do
		ps <- readIORef penState
		when (doesPenDown ps) $
			case buf of
				BG -> Base.lineToBG w x1 y1 x2 y2
				UndoBuf -> Base.lineToUndoBuf w x1 y1 x2 y2
	act BG
	dir <- readIORef world >>= Base.getCursorDir . Base.tWorld
	putToPastDrawLines (x2, y2) dir act

redrawLines :: IO ()
redrawLines = do
	w <- fmap Base.tWorld $ readIORef world
	dls <- fmap (map fromJust . filter isJust) $ readIORef pastDrawLines
	clear
	flip mapM_ dls $ \dl -> do
		snd dl BG
		Base.drawWorld w
		Base.flushWorld $ Base.wWin w
		threadDelay 20000

undoAll :: IO ()
undoAll = do
	w <- fmap Base.tWorld $ readIORef world
	dls <- fmap (map fromJust . filter isJust) $ readIORef pastDrawLines
	flip mapM_ (zip (reverse $ map fst dls) $ map sequence_ $ reverse $ inits
		$ map (($ BG) . snd) dls)
		$ \((pos, dir), dl) -> do
		Base.cleanBG (Base.wWin w)
		dl
		uncurry (Base.setCursorPos w) pos
		Base.setCursorDir w dir
		Base.drawWorld w
		Base.flushWorld $ Base.wWin w
		threadDelay 20000

undo :: IO ()
undo = do
	w <- fmap Base.tWorld $ readIORef world
	dls <- fmap init $ readIORef pastDrawLines
	let	draw = map (map fromJust . filter isJust)
			$ takeWhile (isJust . last) $ reverse $ inits dls
		draw1 = map fromJust $ filter isJust $ head
			$ dropWhile (isJust . last) $ reverse $ inits dls
		draw' = draw ++ [draw1]
	flip mapM_ (zip (reverse $ map (fst . fromJust) $ filter isJust dls )
		$ map sequence_
		$ map (map (($ BG) . snd)) draw') $ \((pos, dir), dl) -> do
		Base.cleanBG (Base.wWin w)
		Base.undoBufToBG (Base.wWin w)
		dl
		uncurry (Base.setCursorPos w) pos
		Base.setCursorDir w dir
		Base.drawWorld w
		Base.flushWorld (Base.wWin w)
		threadDelay 20000
	modifyIORef pastDrawLines $ reverse . dropWhile isJust . tail . reverse
	pushTurtleEvent Undo

rotateBy :: Double -> IO ()
rotateBy dd = do
	w <- fmap Base.tWorld $ readIORef world
	d0 <- Base.getCursorDir w
	let	nd = (d0 + dd) `gMod` 360
	Base.setCursorDir w nd
	Base.drawWorld w
	Base.flushWorld $ Base.wWin w
	pos <- Base.getCursorPos w
	putToPastDrawLines pos nd $ const (return ())

rotateTo :: Double -> IO ()
rotateTo d = do
	w <- fmap Base.tWorld $ readIORef world
	d0 <- Base.getCursorDir w
	let	step = 5
		dd = d - d0
	replicateM_ (abs dd `gDiv` step) $
		rotateBy (signum dd * step) >> threadDelay 10000
	Base.setCursorDir w d
	Base.drawWorld w
	Base.flushWorld $ Base.wWin w

rotate, rawRotate :: Double -> IO ()
rotate d = rawRotate d >> setUndoPoint >> pushTurtleEvent (Rotate d)
rawRotate d = do
	w <- fmap Base.tWorld $ readIORef world
	d0 <- Base.getCursorDir w
	rotateTo $ d0 + d

gDiv :: (Num a, Ord a, Integral b) => a -> a -> b
x `gDiv` y
	| x >= y = 1 + (x - y) `gDiv` y
	| otherwise = 0

gMod :: (Num a, Ord a) => a -> a -> a
x `gMod` y
	| x >= y = (x - y) `gMod` y
	| otherwise = x

right :: Double -> IO ()
right = rotate

left :: Double -> IO ()
left = rotate . negate

circle :: Double -> IO ()
circle r = replicateM_ 36 $ do
	rawForward $ (2 * r * pi / 36 :: Double)
	rawRotate (- 10)

home :: IO ()
home = goto' 0 0 >> rotateTo 0 >> pushTurtleEvent Home

clear :: IO ()
clear = do
	w <- fmap Base.tWorld $ readIORef world
	Base.cleanBG (Base.wWin w)
	Base.drawWorld w >> Base.flushWorld (Base.wWin w)
	pos <- Base.getCursorPos w
	dir <- Base.getCursorDir w
	putToPastDrawLines pos dir $ \buf ->
		case buf of
			BG -> Base.cleanBG $ Base.wWin w
			UndoBuf -> Base.cleanUndoBuf $ Base.wWin w
	pushTurtleEvent Clear
