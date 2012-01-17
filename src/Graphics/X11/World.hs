module Graphics.X11.World (
	World,
	openWorld,
	drawWorld,
	winSize,
	undoBufToBG,
	makeFilledPolygon,
	lineBG,
	cleanBG,
	lineUndoBuf,
	cleanUndoBuf
) where

import qualified Graphics.X11.Window as Win
import Data.IORef

type World = Win

openWorld :: (World -> IO ()) -> IO World
openWorld exposeAction = do
	(win, forExpose) <- Win.openWin
	writeIORef forExpose $ exposeAction win
	return win

drawWorld :: IORef (Win -> Double -> Double -> Double -> Double -> IO ()) ->
	IORef (Double, Double) -> IORef Double -> IORef Double -> World -> IO ()
drawWorld rshape rpos rd rs w = do
	Win.bgToBuf w
	(x, y) <- readIORef rpos
	d <- readIORef rd
	s <- readIORef rs
	displayCursor <- readIORef rshape
	displayCursor w s d x y
	Win.bufToWin w
	flushWorld w

winSize :: Win -> IO (Double, Double)
winSize = Win.winSize

cleanBG, cleanUndoBuf :: Win -> IO ()
cleanBG = Win.clearBG
cleanUndoBuf = Win.clearUndoBuf

lineUndoBuf, lineBG :: Win -> Double -> Double -> Double -> Double -> IO ()
lineBG = Win.lineBG
lineUndoBuf = Win.lineUndoBuf

makeFilledPolygon :: Win -> [(Double, Double)] -> IO ()
makeFilledPolygon = Win.fillPolygonBuf

flushWorld :: Win -> IO ()
flushWorld = Win.flushWin

undoBufToBG :: Win -> IO ()
undoBufToBG = Win.undoBufToBG

type Win = Win.Win
