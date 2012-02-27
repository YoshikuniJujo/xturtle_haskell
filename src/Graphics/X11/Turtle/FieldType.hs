module Graphics.X11.Turtle.FieldType(
	Field,
	initialField,

	fDisplay,
	fWindow,
	fGC,
	fGCBG,
	fDel,
	fEnd,
	fClose,
	fRunning,
	fUndoBuf,
	fBG,
	fBuf,

	fLayers,
	fKeypress,
	fOnclick,
	fOnrelease,
	fOndrag,
	fPress,
	setWinSize,

	onclick,
	onrelease,
	ondrag,
	onkeypress,
	addThread,
	winSize,
	waitField,
	withLock,
	withLock2,
	addLayer,
	addCharacter,
	clearCharacter,
	closeField,
	convertPos,
	convertPosRev,
	fieldSize,

	module Graphics.X11.Turtle.Layers
) where

import Data.IORef
import Control.Concurrent
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Graphics.X11.Turtle.Layers(
	Layers, Layer, Character, setCharacter, newLayers, addLayerAction,
	undoLayer, clearLayer, redrawLayers)
import qualified Graphics.X11.Turtle.Layers as L
import Foreign.C.Types
import Control.Arrow((***))

data Field = Field{
	fDisplay :: Display,
	fWindow :: Window,
	fGC :: GC,
	fGCBG :: GC,
	fDel :: Atom,
	fUndoBuf :: Pixmap,
	fBG :: Pixmap,
	fBuf :: Pixmap,
	fWidth :: IORef Dimension,
	fHeight :: IORef Dimension,

	fLayers :: IORef Layers,

	fWait :: Chan (),
	fWait2 :: Chan (),
	fEvent :: Chan (Maybe Event),
	fClose :: Chan (),
	fRunning :: IORef [ThreadId],
	fOnclick :: IORef (Int -> Double -> Double -> IO Bool),
	fOnrelease :: IORef (Int -> Double -> Double -> IO Bool),
	fOndrag :: IORef (Double -> Double -> IO ()),
	fPress :: IORef Bool,
	fKeypress :: IORef (Char -> IO Bool),
	fEnd :: Chan ()
 }

onclick, onrelease :: Field -> (Int -> Double -> Double -> IO Bool) -> IO ()
onclick f = writeIORef $ fOnclick f
onrelease f = writeIORef $ fOnrelease f

ondrag :: Field -> (Double -> Double -> IO ()) -> IO ()
ondrag f = writeIORef $ fOndrag f

onkeypress :: Field -> (Char -> IO Bool) -> IO ()
onkeypress f = writeIORef $ fKeypress f

addThread :: Field -> ThreadId -> IO ()
addThread f tid = modifyIORef (fRunning f) (tid :)

setWinSize :: Field -> Dimension -> Dimension -> IO ()
setWinSize f w h = do
	writeIORef (fWidth f) w
	writeIORef (fHeight f) h

winSize :: Field -> IO (Dimension, Dimension)
winSize f = do
	width <- readIORef $ fWidth f
	height <- readIORef $ fHeight f
	return (width, height)

waitField :: Field -> IO ()
waitField = readChan . fEnd

withLock :: (Field -> IO a) -> Field -> IO a
withLock act f = do
	readChan $ fWait f
	ret <- act f
	writeChan (fWait f) ()
	return ret

withLock2 :: (Field -> IO a) -> Field -> IO a
withLock2 act f = do
	readChan $ fWait2 f
	ret <- act f
	writeChan (fWait2 f) ()
	return ret

addLayer :: Field -> IO Layer
addLayer = L.addLayer . fLayers

addCharacter :: Field -> IO Character
addCharacter = L.addCharacter . fLayers

clearCharacter :: Character -> IO ()
clearCharacter c = setCharacter c $ return ()

closeField :: Field -> IO ()
closeField f = do
	readIORef (fRunning f) >>= mapM_ killThread
	writeChan (fClose f) ()

convertPos :: Field -> [(Double, Double)] -> IO [(Position, Position)]
convertPos f ps = do
	(width, height) <- fieldSize f
	return $ (round . (+ width / 2) *** round . (+ height / 2) . negate)
		`map` ps

convertPosRev :: Field -> CInt -> CInt -> IO (Double, Double)
convertPosRev f x y = do
	(width, height) <- fieldSize f
	return (fromIntegral x - width / 2, fromIntegral (- y) + height / 2)

fieldSize :: Field -> IO (Double, Double)
fieldSize w = fmap (fromIntegral *** fromIntegral) $ winSize w

initialField :: Display -> Window -> GC -> GC -> Atom ->
	IORef Dimension -> IORef Dimension -> [Pixmap] -> IORef Layers -> IO Field
initialField dpy win gc gcBG del widthRef heightRef bufs fll = do
	wait <- newChan
	wait2 <- newChan
	event <- newChan
	close <- newChan
	running <- newIORef []
	onclickRef <- newIORef $ const $ const $ const $ return True
	onreleaseRef <- newIORef $ const $ const $ const $ return True
	ondragRef <- newIORef $ const $ const $ return ()
	pressRef <- newIORef False
	keypressRef <- newIORef $ const $ return True
	endRef <- newChan
	writeChan wait ()
	writeChan wait2 ()
	return Field{
		fDisplay = dpy,
		fWindow = win,
		fGC = gc,
		fGCBG = gcBG,
		fDel = del,
		fUndoBuf = head bufs,
		fBG = bufs !! 1,
		fBuf = bufs !! 2,
		fWidth = widthRef,
		fHeight = heightRef,
		fWait = wait,
		fWait2 = wait2,
		fEvent = event,
		fClose = close,
		fRunning = running,
		fOnclick = onclickRef,
		fOnrelease = onreleaseRef,
		fOndrag = ondragRef,
		fPress = pressRef,
		fKeypress = keypressRef,
		fEnd = endRef,
		fLayers = fll
	 }
