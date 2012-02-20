module Graphics.X11.TurtleState (
	TurtleState(..),
	initialTurtleState,
--	Color,
	pencolor',
	pencolor,
	setPencolor,
	SVG(..),
--	Draw(..),
	Position(..),
	Color(..),
	colorToWord32
) where

import Data.Word(Word32)
import Data.Bits
import Text.XML.YJSVG

{-
type Color = Word32

data Draw
	= NoDraw
	| Line{
		color :: Color,
		width :: Double,
		begin :: (Double, Double),
		end :: (Double, Double)
	 }
	| Str{
		scolor :: (Double, Double, Double),
		font :: String,
		size :: Double,
		pos :: (Double, Double),
		contents :: String
	 }
	deriving (Show, Eq)
-}

pencolor' :: TurtleState -> Color
pencolor' t =
	RGB (round $ red t * 0xff) (round $ green t * 0xff) (round $ blue t *  0xff)

pencolor :: TurtleState -> Word32
pencolor t = c
	where
	c = shift (round $ red t * 0xff) 16 .|.
		shift (round $ green t * 0xff) 8 .|. round (blue t * 0xff)

colorToWord32 :: Color -> Word32
colorToWord32 (RGB r g b) =
	shift (fromIntegral r) 16 .|. shift (fromIntegral g) 8 .|. fromIntegral b
colorToWord32 _ = error "colorToWord32 (ColorName _) is not implemented"

setPencolor :: TurtleState -> Word32 -> TurtleState
setPencolor t c = t{red = r, green = g, blue = b}
	where
	r_ = shiftR c 16 .&. 0xff
	g_ = shiftR c 8 .&. 0xff
	b_ = c .&. 0xff
	[r, g, b] = map ((/ 0xff) . fromIntegral) [r_, g_, b_]

data TurtleState = TurtleState {
	position :: (Double, Double),
	direction :: Double,
	degrees :: Double,
	pendown :: Bool,
	pensize :: Double,
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
