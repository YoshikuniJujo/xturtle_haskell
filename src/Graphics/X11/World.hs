module Graphics.X11.World (
--	World(wWin),
	World,
	wWin,
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

data World = World{wWin :: Win}
-- type World = Win

-- wWin :: World -> Win
-- wWin = id

openWorld :: (World -> IO ()) -> IO World
openWorld exposeAction = do
	(win, forExpose) <- Win.openWin
	let world = World win
	writeIORef forExpose $ exposeAction world
	return world

drawWorld ::
	IORef (Win -> Double -> Double -> Double -> Double -> IO ()) ->
		IORef (Double, Double) -> IORef Double -> IORef Double ->
		World -> IO ()
drawWorld rshape rpos rd rs w = do
	Win.bgToBuf $ wWin w
	(x, y) <- readIORef rpos
	d <- readIORef rd
	s <- readIORef rs
	displayCursor <- readIORef rshape
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
