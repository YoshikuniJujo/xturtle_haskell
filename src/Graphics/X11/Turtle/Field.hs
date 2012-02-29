module Graphics.X11.Turtle.Field(
	Field,
	withLock2,

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
	flushWindow,

	onclick,
	onrelease,
	ondrag,
	onkeypress,

	forkIOX,
	addThread
) where

import Data.IORef
import Graphics.X11.Xlib.Extras(Event(..), getEvent)
import Graphics.X11.Xim

import Data.Maybe

import Control.Concurrent(
	forkIO, ThreadId, Chan, newChan, writeChan, readChan, threadWaitRead,
	killThread)

import System.Posix.Types

import Graphics.X11 hiding (Color, drawLine)

import Text.XML.YJSVG(Color(..))
import Control.Monad
import Control.Monad.Tools

import Graphics.X11.Turtle.Layers(
	Layers, Layer, Character, setCharacter, newLayers, addLayerAction,
	undoLayer, clearLayer, redrawLayers)
import qualified Graphics.X11.Turtle.Layers as L
import Graphics.X11.Turtle.XTools
import Foreign.C.Types
import Control.Arrow((***))
import Data.Convertible(convert)

openField :: IO Field
openField = do
	(dpy, win, gc, gcBG, bufs@[undoBuf, bg, buf], ic, del, width, height)
		<- openWindow
	widthRef <- newIORef width
	heightRef <- newIORef height
	let size = do
		w <- readIORef widthRef
		h <- readIORef heightRef
		return (w, h)
	fll <- newLayers 50 
		(size >>= uncurry (fillRectangle dpy undoBuf gcBG 0 0))
		(size >>= \(w, h) -> copyArea dpy undoBuf bg gc 0 0 w h 0 0)
		(size >>= \(w, h) -> copyArea dpy bg buf gc 0 0 w h 0 0)
	f <- initialField dpy win gc gcBG del widthRef heightRef bufs fll
	_ <- forkIOX $ runLoop ic f
	flushWindow f
	return f

waitInput :: Field -> Chan () -> IO (Chan Bool)
waitInput f t = do
	c <- newChan
	tid <- forkIOX $ forever $ do
		threadWaitRead $ getConnection f
		writeChan c False
		readChan t
	_ <- forkIO $ do
		readChan $ fClose f
		writeChan c True
	addThread f tid
	return c
	where
	getConnection = Fd . connectionNumber . fDisplay

runLoop :: XIC -> Field -> IO ()
runLoop ic f = allocaXEvent $ \e -> do
	timing <- newChan
	endc <- waitInput f timing
	doWhile_ $ do
		end <- readChan endc
		cont <- doWhile True $ \_ -> do
			evN <- pending $ fDisplay f
			if evN > 0 then do
					nextEvent (fDisplay f) e
					ret <- ifM (filterEvent e 0)
							(return True)
							(do	ev <- getEvent e
								eventFun f ic e ev)
					return (ret, ret)
				else return (True, False)
		writeChan timing ()
		unless (not end && cont) $
			readIORef (fRunning f) >>= mapM_ killThread
		return $ not end && cont
	destroyWindow (fDisplay f) (fWindow f)
	closeDisplay $ fDisplay f
	informEnd f

eventFun :: Field -> XIC -> XEventPtr -> Event -> IO Bool
eventFun f ic e ev = case ev of
	ExposeEvent{} -> exposeFun f
	KeyEvent{} -> keyFun f ic e
	ButtonEvent{} -> buttonFun f ev
	MotionEvent{} -> motionFun f ev
	ClientMessageEvent{} -> return $ isWMDelete f ev
	_ -> return True

exposeFun :: Field -> IO Bool
exposeFun f = do
	(_, _, _, width, height, _, _) <- getGeometry (fDisplay f) (fWindow f)
	setWinSize f width height
	redrawLayers $ fLayers f
	flushWindow f
	return True

keyFun :: Field -> XIC -> XEventPtr -> IO Bool
keyFun f ic e = do
	(mstr, mks) <- utf8LookupString ic e
	let	str = fromMaybe "" mstr
		_ks = fromMaybe xK_VoidSymbol mks
	readIORef (fKeypress f) >>= fmap and . ($ str) . mapM

buttonFun :: Field -> Event -> IO Bool
buttonFun f ev = do
	pos <- convertPosRev f (ev_x ev) (ev_y ev)
	case ev_event_type ev of
		et	| et == buttonPress -> do
				writeIORef (fPress f) True
				fun <- readIORef (fOnclick f)
				uncurry (fun $ fromIntegral $ ev_button ev) pos
			| et == buttonRelease -> do
				writeIORef (fPress f) False
				fun <- readIORef (fOnrelease f)
				uncurry (fun $ fromIntegral $ ev_button ev) pos
		_ -> error "not implement event"

motionFun :: Field -> Event -> IO Bool
motionFun f ev = do
	pos <- convertPosRev f (ev_x ev) (ev_y ev)
	whenM (readIORef $ fPress f) $ readIORef (fOndrag f) >>= ($ pos) . uncurry
	return True

flushWindow :: Field -> IO ()
flushWindow = withLock $ \f -> do
	(width, height) <- winSize f
	copyArea (fDisplay f) (fBuf f) (fWindow f) (fGC f) 0 0 width height 0 0
	flush $ fDisplay f

drawLine :: Field -> Layer -> Double -> Color ->
	Double -> Double -> Double -> Double -> IO ()
drawLine f l lw clr x1 y1 x2 y2 =
	addLayerAction l (drawLineBuf f (round lw) clr fUndoBuf x1 y1 x2 y2,
		drawLineBuf f (round lw) clr fBG x1 y1 x2 y2)

writeString :: Field -> Layer -> String -> Double -> Color ->
	Double -> Double -> String -> IO ()
writeString f l fname size clr x_ y_ str =
	addLayerAction l (writeStringBuf fUndoBuf, writeStringBuf fBG)
	where
	writeStringBuf buf = do
		(x, y) <- convertPos f x_ y_
		writeStringBase (fDisplay f) (buf f) fname size clr x y str

drawCharacter :: Field -> Character -> Color -> [(Double, Double)] -> IO ()
drawCharacter f c cl sh = setCharacter c $ do
	clr <- getColorPixel (fDisplay f) cl
	setForeground (fDisplay f) (fGC f) clr
	fillPolygonBuf f sh

drawCharacterAndLine ::	Field -> Character -> Color -> [(Double, Double)] -> Double ->
	Double -> Double -> Double -> Double -> IO ()
drawCharacterAndLine f c cl ps lw x1 y1 x2 y2 = setCharacter c $ do
	clr <- getColorPixel (fDisplay f) cl
	setForeground (fDisplay f) (fGC f) clr
	fillPolygonBuf f ps >> drawLineBuf f (round lw) cl fBuf x1 y1 x2 y2

drawLineBuf :: Field -> Int -> Color -> (Field -> Pixmap) ->
	Double -> Double -> Double -> Double -> IO ()
drawLineBuf f lw c bf x1_ y1_ x2_ y2_ = do
	(x1, y1) <- convertPos f x1_ y1_
	(x2, y2) <- convertPos f x2_ y2_
	drawLineBase (fDisplay f) (fGC f) (bf f) lw c x1 y1 x2 y2

fillPolygonBuf :: Field -> [(Double, Double)] -> IO ()
fillPolygonBuf f ps_ = do
	ps <- mapM (uncurry $ convertPos f) ps_
	fillPolygon (fDisplay f) (fBuf f) (fGC f) (map (uncurry Point) ps)
		nonconvex coordModeOrigin

fieldColor :: Field -> Color -> IO ()
fieldColor f c = do
	clr <- getColorPixel (fDisplay f) c
	setForeground (fDisplay f) (fGCBG f) clr
	(width, height) <- winSize f
	forM_ [fUndoBuf f, fBG f, fBuf f] $ \bf ->
		fillRectangle (fDisplay f) bf (fGCBG f) 0 0 width height

---------------------------------------------------------------------------

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

isWMDelete :: Field -> Event -> Bool
isWMDelete f ev = convert (head $ ev_data ev) /= fDel f

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

informEnd :: Field -> IO ()
informEnd = flip writeChan () . fEnd

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

convertPos :: Field -> Double -> Double -> IO (Position, Position)
convertPos f x y = do
	(width, height) <- fieldSize f
	return $ (round $ x + width / 2, round $ - y + height / 2)

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
