module Graphics.X11.Turtle.Data(
	-- * tables
	shapeTable, speedTable
) where

import Control.Arrow(second, (&&&))

shapeTable :: [(String, [(Double, Double)])]
shapeTable = [
	("classic", unfold [(- 10, 0), (- 16, 6), (0, 0)]),
	("turtle", unfold [
		(- 10, 0), (- 8, 3), (- 10, 5), (- 7, 9), (- 5, 6), (0, 8),
		(4, 7), (6, 10), (8, 7), (7, 5), (10, 2), (13, 3), (16, 0)])]
	where unfold = uncurry (++) . (id &&& (reverse . map (second negate)))

speedTable :: [(String, (Maybe Double, Maybe Double))]
speedTable = [
	("fastest", (Nothing, Nothing)),
	("fast", (Just 60, Just $ pi / 3)),
	("normal", (Just 20, Just $ pi / 9)),
	("slow", (Just 10, Just $ pi / 18)),
	("slowest", (Just 3, Just $ pi / 60))]
