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

type World = Win

drawWorld :: World -> (World -> IO ()) -> IO ()
drawWorld w act = bgToBuf w >> act w >> bufToWin w >> flushWin w

openWorld :: IO World
openWorld = openWin

drawCursor :: World -> [(Double, Double)] -> IO ()
drawCursor = fillPolygonBuf
