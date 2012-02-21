module TurtleState (
	TurtleState(..),
	initialTurtleState,
	pencolor',
	pencolor_,
	setPencolor,
	SVG(..),
	Position(..),
	Color(..),
	colorToWord32
) where

import Data.Word(Word32)
import Data.Bits
import Text.XML.YJSVG

pencolor' :: TurtleState -> Color
pencolor' t =
	RGB (round $ red t * 0xff) (round $ green t * 0xff) (round $ blue t *  0xff)

pencolor_ :: TurtleState -> Word32
pencolor_ t = c
	where
	c = shift (round $ red t * 0xff) 16 .|.
		shift (round $ green t * 0xff) 8 .|. round (blue t * 0xff)

colorToWord32 :: Color -> Word32
colorToWord32 (RGB r g b) =
	shift (fromIntegral r) 16 .|. shift (fromIntegral g) 8 .|. fromIntegral b
colorToWord32 _ = error "colorToWord32 (ColorName _) is not implemented"

setPencolor :: TurtleState -> Word32 -> TurtleState
setPencolor t c = t{red = r, green = g, blue = b, pencolor = RGB r_ g_ b_}
	where
	r_ = fromIntegral $ shiftR c 16 .&. 0xff
	g_ = fromIntegral $ shiftR c 8 .&. 0xff
	b_ = fromIntegral $ c .&. 0xff
	[r, g, b] = map ((/ 0xff) . fromIntegral) [r_, g_, b_]

data TurtleState = TurtleState {
	position :: (Double, Double),
	direction :: Double,
	degrees :: Double,
	pendown :: Bool,
	pensize :: Double,
	pencolor :: Color,
	red :: Double,
	green :: Double,
	blue :: Double,
	shape :: [(Double, Double)],
	shapesize :: Double,
	visible :: Bool,
	clear :: Bool,
	undo :: Bool,
	line :: Bool,
	undonum :: Int,
	draw :: Maybe SVG,
	drawed :: [SVG]
 } deriving Show

initialTurtleState :: [(Double, Double)] -> TurtleState
initialTurtleState sh = TurtleState {
	position = (0, 0),
	direction = 0,
	degrees = 360,
	pendown = True,
	pensize = 0,
	pencolor = RGB 0 0 0,
	red = 0,
	green = 0,
	blue = 0,
	shape = sh,
	shapesize = 1,
	visible = True,
	clear = False,
	undo = False,
	line = False,
	undonum = 1,
	draw = Nothing,
	drawed = []
 }
