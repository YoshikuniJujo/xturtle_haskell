module Graphics.X11.World (
	World(wWin, wShape),
	Win,
	openWorld,
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
	wShape :: IORef (Win -> Double -> Double -> Double -> Double -> IO ())
 }

openWorld :: (World -> IO ()) -> IO World
openWorld exposeAction = do
	(win, forExpose) <- Win.openWin
	initShape <- newIORef undefined
	let world = World win initShape
	writeIORef forExpose $ exposeAction world
	return world

drawWorld :: IORef (Double, Double) -> IORef Double -> IORef Double -> World -> IO ()
drawWorld rpos rd rs w = do
	Win.bgToBuf $ wWin w
	(x, y) <- readIORef rpos -- $ wPos w
	d <- readIORef rd -- $ wDir w
	s <- readIORef rs -- $ wSize w
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
