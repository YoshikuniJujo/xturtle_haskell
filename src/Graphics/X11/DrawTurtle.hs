module Graphics.X11.DrawTurtle (
	Field,
	Layer,
	Character,

	openField,
	addCharacter,
	addLayer,
	drawTurtle,
	line,
	undoLayer,

	classic,
	turtle
) where

import Graphics.X11.WindowLayers
import Graphics.X11.MakeTurtle

drawTurtle :: Field -> Character -> [(Double, Double)] -> Double -> Double ->
	(Double, Double) -> Maybe (Double, Double) -> IO ()
drawTurtle w c sh s d (x, y) org = do
	let sp = mkShape sh s d x y
	maybe (setPolygonCharacter w c sp)
		(flip (setPolygonCharacterAndLine w c sp) (x, y)) org
	bufToWin w
	flushWin w
