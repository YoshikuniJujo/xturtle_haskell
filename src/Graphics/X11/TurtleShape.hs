module Graphics.X11.TurtleShape(
	lookupShape,
	classic
 ) where

import Control.Arrow(second)
import Data.Maybe(fromMaybe)

lookupShape :: String -> [(Double, Double)]
lookupShape sn = fromMaybe (error errMsg) $ lookup sn shapeList
	where errMsg = "There is no shape named " ++ sn

shapeList :: [(String, [(Double, Double)])]
shapeList = [
	("classic", classic),
	("turtle", turtle)
 ]

classic :: [(Double, Double)]
classic = clssc ++ reverse (map (second negate) clssc)
	where
	clssc = [
		(- 10, 0),
		(- 16, 6),
		(0, 0)
	 ]

turtle :: [(Double, Double)]
turtle = ttl ++ reverse (map (second negate) ttl)
	where
	ttl = [
		(- 10, 0),
		(- 8, - 3),
		(- 10, - 5),
		(- 7, - 9),
		(- 5, - 6),
		(0, - 8),
		(4, - 7),
		(6, - 10),
		(8, - 7),
		(7, - 5),
		(10, - 2),
		(13, - 3),
		(16, 0)
	 ]
