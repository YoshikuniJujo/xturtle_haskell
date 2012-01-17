module Graphics.X11.World (
	World,
	openWorld,
	drawWorld,
	winSize,
	undoBufToBG,
	drawCursor,
	lineBG,
	clearBG,
	lineUndoBuf,
	clearUndoBuf,

	addExposeAction
) where

import Graphics.X11.Window
import Data.IORef

type World = Win

openWorld :: IO World
openWorld = do
	(win, forExpose) <- openWin
	return win

drawWorld :: IORef (Win -> Double -> Double -> Double -> Double -> IO ()) ->
	IORef (Double, Double) -> IORef Double -> IORef Double -> World -> IO ()
drawWorld rshape rpos rd rs w = do
	bgToBuf w
	(x, y) <- readIORef rpos
	d <- readIORef rd
	s <- readIORef rs
	displayCursor <- readIORef rshape
	displayCursor w s d x y
	bufToWin w
	flushWorld w

drawCursor :: World -> [(Double, Double)] -> IO ()
drawCursor = fillPolygonBuf

flushWorld :: Win -> IO ()
flushWorld = flushWin
