module Graphics.X11.Turtle.Shape(lookupShape) where

import Control.Arrow(second, (&&&))

lookupShape :: String -> [(Double, Double)]
lookupShape "classic" = unfold [(- 10, 0), (- 16, 6), (0, 0)]
lookupShape "turtle" = unfold [
	(- 10, 0), (- 8, 3), (- 10, 5), (- 7, 9), (- 5, 6), (0, 8), (4, 7),
	(6, 10), (8, 7), (7, 5), (10, 2), (13, 3), (16, 0)
 ]
lookupShape name = error $ "There is no shape named " ++ name

unfold :: [(Double, Double)] -> [(Double, Double)]
unfold = uncurry (++) . (id &&& (reverse . map (second negate)))
