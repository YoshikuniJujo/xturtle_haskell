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
	closeWorld,
	flushWorld,
	makeFilledPolygonCursor,
	lineToBG,
	lineToUndoBuf,
	cleanBG,
	cleanUndoBuf,
	getWindowSize,
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
setCursorDir w d = writeIORef (wDir w) d

getCursorDir :: World -> IO Double
getCursorDir w = readIORef (wDir w)

setCursorSize :: World -> Double -> IO ()
setCursorSize w s = writeIORef (wSize w) s

setCursorShape ::
	World -> (Win -> Double -> Double -> Double -> Double -> IO ()) -> IO ()
setCursorShape w s = writeIORef (wShape w) s

openWorld :: IO World
openWorld = do
	(win, forExpose) <- Win.openWindow
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

getWindowSize = Win.getWindowSize
cleanBG = Win.cleanBG
cleanUndoBuf = Win.cleanUndoBuf
lineToBG = Win.lineToBG
lineToUndoBuf = Win.lineToUndoBuf
makeFilledPolygonCursor = Win.makeFilledPolygonCursor
flushWorld = Win.flushWindow
closeWorld = Win.closeWindow
undoBufToBG = Win.undoBufToBG
type Win = Win.Win
