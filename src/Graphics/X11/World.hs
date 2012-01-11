module Graphics.X11.World (
	World,
	wWin,
	Win,
	openWorld,
	setCursorPos,
	getCursorPos,
	setCursorDir,
	getCursorDir,
	setCursorSize,
	setCursorShape,
	drawWorld,
	undoBufToBG,
	flushWorld,
	makeFilledPolygonCursor,
	lineToBG,
	lineToUndoBuf,
	cleanBG,
	cleanUndoBuf,
	winSize
) where

import qualified Graphics.X11.Window as Win
import Data.IORef

data World = World {
	wWin :: Win,
	wPos :: IORef (Double, Double),
	wDir :: IORef Double,
	wSize :: IORef Double,
	wShape :: IORef (Win -> Double -> Double -> Double -> Double -> IO ())
 }

setCursorPos :: World -> Double -> Double -> IO ()
setCursorPos w x y = writeIORef (wPos w) (x, y)

getCursorPos :: World -> IO (Double, Double)
getCursorPos w = readIORef (wPos w)

setCursorDir :: World -> Double -> IO ()
setCursorDir = writeIORef . wDir

getCursorDir :: World -> IO Double
getCursorDir w = readIORef (wDir w)

setCursorSize :: World -> Double -> IO ()
setCursorSize = writeIORef . wSize

setCursorShape ::
	World -> (Win -> Double -> Double -> Double -> Double -> IO ()) -> IO ()
setCursorShape = writeIORef . wShape

openWorld :: IO World
openWorld = do
	(win, forExpose) <- Win.openWin
	initPos <- newIORef undefined
	initDir <- newIORef undefined
	initSize <- newIORef undefined
	initShape <- newIORef undefined
	let world = World win initPos initDir initSize initShape
	writeIORef forExpose $ drawWorld world
	return world

drawWorld :: World -> IO ()
drawWorld w = do
	Win.bgToBuf $ wWin w
	(x, y) <- readIORef $ wPos w
	d <- readIORef $ wDir w
	s <- readIORef $ wSize w
	displayCursor <- readIORef $ wShape w
	displayCursor (wWin w) s d x y
	Win.bufToWin $ wWin w

winSize :: Win -> IO (Double, Double)
winSize = Win.winSize

cleanBG, cleanUndoBuf :: Win -> IO ()
cleanBG = Win.clearBG
cleanUndoBuf = Win.clearUndoBuf

lineToUndoBuf, lineToBG :: Win -> Double -> Double -> Double -> Double -> IO ()
lineToBG = Win.lineBG
lineToUndoBuf = Win.lineUndoBuf

makeFilledPolygonCursor :: Win -> [(Double, Double)] -> IO ()
makeFilledPolygonCursor = Win.fillPolygonBuf

flushWorld :: Win -> IO ()
flushWorld = Win.flushWin

undoBufToBG :: Win -> IO ()
undoBufToBG = Win.undoBufToBG

type Win = Win.Win
