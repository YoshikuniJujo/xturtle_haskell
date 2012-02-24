module Graphics.X11.Turtle.Field2(
	Field,
	Layer,
	Character,
	openField,
	closeField,
	waitField,
	fieldColor,
	fieldSize,
	addLayer,
	addCharacter,
	drawLine,
	writeString,
	drawCharacter,
	drawCharacterAndLine,
	clearCharacter,
	undoLayer,
	clearLayer,
	flushLayer,
	onclick,
	onrelease,
	ondrag,
	onkeypress,
	forkIOX,
	addThread
) where

import Graphics.X11.Turtle.X11
import Graphics.X11.Turtle.Layers
import Text.XML.YJSVG(Color)
import Control.Concurrent
import Data.IORef
import Control.Arrow

data Field = Field{
	fDisplay :: Display,
	fWindow :: Window,
	fGC :: GC,
	fCharacters :: IORef [IO ()]
 }

data Layer = Layer

data Character = Character{characterField :: Field, characterId :: Int}

openField :: IO Field
openField = do
	charsRef <- newIORef []
	dpy <- openDisplay ""
	let	scr = defaultScreen dpy
		black = blackPixel dpy scr
		white = whitePixel dpy scr
	root <- rootWindow dpy scr
	win <- createSimpleWindow dpy root 0 0 100 100 1 black white
	gc <- createGC dpy win
	mapWindow dpy win
	flush dpy
	return Field{
		fDisplay = dpy,
		fWindow = win,
		fGC = gc,
		fCharacters = charsRef
	 }

closeField :: Field -> IO ()
closeField f = do
	closeDisplay $ fDisplay f

waitField :: Field -> IO ()
waitField f = return ()

fieldColor :: Field -> Color -> IO ()
fieldColor f c = return ()

fieldSize :: Field -> IO (Double, Double)
fieldSize f = return (0, 0)

addLayer :: Field -> IO Layer
addLayer f = return Layer

addCharacter :: Field -> IO Character
addCharacter f = do
	cs <- readIORef $ fCharacters f
	writeIORef (fCharacters f) (cs ++ [return ()])
	return Character{characterField = f, characterId = length cs}

drawLine :: Layer -> Double -> Color -> Double -> Double -> Double -> Double -> IO ()
drawLine l w c x0 y0 x1 y1 = return ()

writeString :: Layer -> String -> Double -> Color -> Double -> Double -> String -> IO ()
writeString l font size c x y str = return ()

drawCharacter :: Character -> Color -> [(Double, Double)] -> IO ()
drawCharacter c@Character{characterField = f} clr sh = do
	ps <- mapM (convertPos f) sh
	fillPolygon (fDisplay f) (fWindow f) (fGC f) (map (uncurry Point) ps)
		nonconvex coordModeOrigin
	flush $ fDisplay f

{-
setCharacter :: Character -> IO () -> IO ()
setCharacter Character{characterField = f, characterId = cid} act = do
	cs <- readIORef $ fCharacters f
-}

convertPos :: Field -> (Double, Double) -> IO (Position, Position)
convertPos f p = do
	(_, _, _, width, height, _, _) <- getGeometry (fDisplay f) (fWindow f)
	return $ (round . (+ fromIntegral width / 2) *** round . (+ fromIntegral height / 2) . negate) p

drawCharacterAndLine :: Character -> Color -> [(Double, Double)] -> Double -> Double ->
	Double -> Double -> Double -> IO ()
drawCharacterAndLine c clr sh w x0 y0 x1 y1 = do
	drawCharacter c clr sh

clearCharacter :: Character -> IO ()
clearCharacter c = return ()

undoLayer :: Layer -> IO Bool
undoLayer l = return True

clearLayer :: Layer -> IO ()
clearLayer l = return ()

flushLayer :: Layer -> IO ()
flushLayer l = return ()

onclick :: Field -> (Int -> Double -> Double -> IO Bool) -> IO ()
onclick f act = return ()

onrelease :: Field -> (Double -> Double -> IO Bool) -> IO ()
onrelease f act = return ()

ondrag :: Field -> (Double -> Double -> IO ()) -> IO ()
ondrag f act = return ()

onkeypress :: Field -> (Char -> IO Bool) -> IO ()
onkeypress f act = return ()

forkIOX :: IO () -> IO ThreadId
forkIOX = (initThreads >>) . forkIO

addThread :: Field -> ThreadId -> IO ()
addThread f tid = return ()
