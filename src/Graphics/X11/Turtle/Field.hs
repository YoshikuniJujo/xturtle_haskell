module Graphics.X11.Turtle.Field(
	-- * types and classes
	Field,
	Layer,
	Character,

	-- * open and close
	openField,
	closeField,
	waitField,
	fieldSize,

	-- * draw
	forkField,
	flushField,
	fieldColor,

	-- ** to Layer
	addLayer,
	drawLine,
	writeString,
	undoLayer,
	clearLayer,

	-- ** to Character
	addCharacter,
	drawCharacter,
	drawCharacterAndLine,
	clearCharacter,

	-- * event driven
	onclick,
	onrelease,
	ondrag,
	onkeypress
) where

import Graphics.X11.Turtle.XTools(
	forkIOX, openWindow, drawLineBase, writeStringBase, getColorPixel,
	Bufs, undoBuf, bgBuf, topBuf,
	GCs, gcForeground, gcBackground, windowSize)
import Graphics.X11.Turtle.Layers(
	Layers, Layer, Character, newLayers, redrawLayers,
	makeLayer, addDraw, undoLayer, clearLayer, makeCharacter, setCharacter)
import Text.XML.YJSVG(Color(..))

import Graphics.X11(
	Display, Window, Pixmap, Atom, Position, Dimension, XEventPtr,
	Point(..), flush, closeDisplay, destroyWindow, copyArea,
	setForeground, fillRectangle, fillPolygon, nonconvex, coordModeOrigin,
	allocaXEvent, pending, nextEvent, buttonPress, buttonRelease,
	xK_VoidSymbol, connectionNumber)
import Graphics.X11.Xlib.Extras(Event(..), getEvent)
import Graphics.X11.Xim(XIC, filterEvent, utf8LookupString)

import Control.Monad(forever, forM_)
import Control.Monad.Tools(doWhile_, doWhile, whenM)
import Control.Arrow((***))
import Control.Concurrent(
	forkIO, ThreadId, threadWaitRead, killThread,
	Chan, newChan, readChan, writeChan)

import Data.IORef(IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Maybe(fromMaybe)
import Data.Convertible(convert)

import System.Posix.Types(Fd(..))
import Foreign.C.Types(CInt)

--------------------------------------------------------------------------------

data Field = Field{
	fDisplay :: Display,
	fWindow :: Window,
	fBufs :: Bufs,
	fGCs :: GCs,
	fIC :: XIC,
	fDel :: Atom,
	fSize :: IORef (Dimension, Dimension),

	fOnclick, fOnrelease :: IORef (Int -> Double -> Double -> IO Bool),
	fOndrag :: IORef (Double -> Double -> IO ()),
	fKeypress :: IORef (Char -> IO Bool),
	fPress :: IORef Bool,

	fLayers :: IORef Layers,
	fRunning :: IORef [ThreadId],
	fLock, fClose, fEnd :: Chan ()
 }

makeField :: Display -> Window -> Bufs -> GCs -> XIC -> Atom ->
	IORef (Dimension, Dimension) -> IORef Layers -> IO Field
makeField dpy win bufs gcs ic del sizeRef fll = do
	lock <- newChan
	close <- newChan
	running <- newIORef []
	onclickRef <- newIORef $ const $ const $ const $ return True
	onreleaseRef <- newIORef $ const $ const $ const $ return True
	ondragRef <- newIORef $ const $ const $ return ()
	pressRef <- newIORef False
	keypressRef <- newIORef $ const $ return True
	endRef <- newChan
	writeChan lock ()
	return Field{
		fDisplay = dpy,
		fWindow = win,
		fGCs = gcs,
		fIC = ic,
		fDel = del,
		fBufs = bufs,
		fSize = sizeRef,
		fLock = lock,
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

openField :: IO Field
openField = do
	(dpy, win, bufs, gcs, ic, del, size) <- openWindow
	let	(ub, bb, tb) = (undoBuf bufs, bgBuf bufs, topBuf bufs)
		(gcf, gcb) = (gcForeground gcs, gcBackground gcs)
	sizeRef <- newIORef size
	let getSize = readIORef sizeRef
	ls <- newLayers 50 
		(getSize >>= uncurry (fillRectangle dpy ub gcb 0 0))
		(getSize >>= \(w, h) -> copyArea dpy ub bb gcf 0 0 w h 0 0)
		(getSize >>= \(w, h) -> copyArea dpy bb tb gcf 0 0 w h 0 0)
	f <- makeField dpy win bufs gcs ic del sizeRef ls
	_ <- forkIOX $ runLoop f
	flush dpy
	return f

waitInput :: Field -> IO (Chan Bool, Chan ())
waitInput f = do
	go <- newChan
	empty <- newChan
	tid <- forkIOX $ forever $ do
		threadWaitRead $ Fd $ connectionNumber $ fDisplay f
		writeChan go True
		readChan empty
	_ <- forkIO $ do
		readChan $ fClose f
		writeChan go False
	modifyIORef (fRunning f) (tid :)
	return (go, empty)

runLoop :: Field -> IO ()
runLoop f = allocaXEvent $ \e -> do
	(go, empty) <- waitInput f
	doWhile_ $ do
		notEnd <- readChan go
		cont <- doWhile True $ const $ do
			evN <- pending $ fDisplay f
			if evN > 0 then do
					nextEvent (fDisplay f) e
					filtered <- filterEvent e 0
					if filtered then return (True, True)
						else do	ev <- getEvent e
							r <- processEvent f e ev
							return (r, r)
				else return (True, False)
		if notEnd && cont then do
				writeChan empty ()
				return True
			else do	readIORef (fRunning f) >>= mapM_ killThread
				return False
	destroyWindow (fDisplay f) (fWindow f)
	closeDisplay $ fDisplay f
	writeChan (fEnd f) ()

processEvent :: Field -> XEventPtr -> Event -> IO Bool
processEvent f e ev = case ev of
	ExposeEvent{} -> flushField f $ do
		windowSize (fDisplay f) (fWindow f) >>= writeIORef (fSize f)
		redrawLayers $ fLayers f
		return True
	KeyEvent{} -> do
		(mstr, mks) <- utf8LookupString (fIC f) e
		let	str = fromMaybe "" mstr
			_ks = fromMaybe xK_VoidSymbol mks
		readIORef (fKeypress f) >>= fmap and . ($ str) . mapM
	ButtonEvent{} -> do
		pos <- fromCenter f (ev_x ev) (ev_y ev)
		let	buttonN = fromIntegral $ ev_button ev
		case ev_event_type ev of
			et	| et == buttonPress -> do
					writeIORef (fPress f) True
					readIORef (fOnclick f) >>=
						($ pos) . uncurry . ($ buttonN)
				| et == buttonRelease -> do
					writeIORef (fPress f) False
					readIORef (fOnrelease f) >>=
						($ pos) . uncurry . ($ buttonN)
			_ -> error "not implement event"
	MotionEvent{} -> do
		pos <- fromCenter f (ev_x ev) (ev_y ev)
		whenM (readIORef $ fPress f) $
			readIORef (fOndrag f) >>= ($ pos) . uncurry
		return True
	ClientMessageEvent{} -> return $ convert (head $ ev_data ev) /= fDel f
	_ -> return True

closeField :: Field -> IO ()
closeField f = do
	readIORef (fRunning f) >>= mapM_ killThread
	writeChan (fClose f) ()

waitField :: Field -> IO ()
waitField = readChan . fEnd

fieldSize :: Field -> IO (Double, Double)
fieldSize = fmap (fromIntegral *** fromIntegral) . readIORef . fSize

forkField :: Field -> IO () -> IO ThreadId
forkField f act = do
	tid <- forkIOX act
	modifyIORef (fRunning f) (tid :)
	return tid

flushField :: Field -> IO a -> IO a
flushField f act = do
	readChan $ fLock f
	ret <- act
	(w, h) <- readIORef $ fSize f
	copyArea (fDisplay f) (topBuf $ fBufs f) (fWindow f) (gcForeground $ fGCs f)
		0 0 w h 0 0
	flush $ fDisplay f
	writeChan (fLock f) ()
	return ret

fieldColor :: Field -> Color -> IO ()
fieldColor f c = do
	clr <- getColorPixel (fDisplay f) c
	setForeground (fDisplay f) (gcBackground $ fGCs f) clr
	(w, h) <- readIORef $ fSize f
	forM_ [undoBuf $ fBufs f, bgBuf $ fBufs f, topBuf $ fBufs f] $ \bf ->
		fillRectangle (fDisplay f) bf (gcBackground $ fGCs f) 0 0 w h

addLayer :: Field -> IO Layer
addLayer = makeLayer . fLayers

drawLine :: Field -> Layer -> Double -> Color ->
	Double -> Double -> Double -> Double -> IO ()
drawLine f l lw clr x1 y1 x2 y2 =
	addDraw l (drawLineBuf f (round lw) clr (undoBuf . fBufs) x1 y1 x2 y2,
		drawLineBuf f (round lw) clr (bgBuf . fBufs) x1 y1 x2 y2)

writeString :: Field -> Layer -> String -> Double -> Color ->
	Double -> Double -> String -> IO ()
writeString f l fname size clr x_ y_ str =
	addDraw l (writeStringBuf $ undoBuf . fBufs, writeStringBuf $ bgBuf . fBufs)
	where
	writeStringBuf bf = do
		(x, y) <- fromTopLeft f x_ y_
		writeStringBase (fDisplay f) (bf f) fname size clr x y str

addCharacter :: Field -> IO Character
addCharacter = makeCharacter . fLayers

drawCharacter :: Field -> Character -> Color -> [(Double, Double)] -> IO ()
drawCharacter f c cl sh = setCharacter c $ do
	clr <- getColorPixel (fDisplay f) cl
	setForeground (fDisplay f) (gcForeground $ fGCs f) clr
	fillPolygonBuf f sh

drawCharacterAndLine ::	Field -> Character -> Color -> [(Double, Double)] -> Double ->
	Double -> Double -> Double -> Double -> IO ()
drawCharacterAndLine f c cl ps lw x1 y1 x2 y2 = setCharacter c $ do
	clr <- getColorPixel (fDisplay f) cl
	setForeground (fDisplay f) (gcForeground $ fGCs f) clr
	fillPolygonBuf f ps >> drawLineBuf f (round lw) cl (topBuf . fBufs) x1 y1 x2 y2

clearCharacter :: Character -> IO ()
clearCharacter c = setCharacter c $ return ()

onclick, onrelease :: Field -> (Int -> Double -> Double -> IO Bool) -> IO ()
onclick f = writeIORef $ fOnclick f
onrelease f = writeIORef $ fOnrelease f

ondrag :: Field -> (Double -> Double -> IO ()) -> IO ()
ondrag f = writeIORef $ fOndrag f

onkeypress :: Field -> (Char -> IO Bool) -> IO ()
onkeypress f = writeIORef $ fKeypress f

drawLineBuf :: Field -> Int -> Color -> (Field -> Pixmap) ->
	Double -> Double -> Double -> Double -> IO ()
drawLineBuf f lw c bf x1_ y1_ x2_ y2_ = do
	(x1, y1) <- fromTopLeft f x1_ y1_
	(x2, y2) <- fromTopLeft f x2_ y2_
	drawLineBase (fDisplay f) (gcForeground $ fGCs f) (bf f) lw c x1 y1 x2 y2

fillPolygonBuf :: Field -> [(Double, Double)] -> IO ()
fillPolygonBuf f ps_ = do
	ps <- mapM (uncurry $ fromTopLeft f) ps_
	fillPolygon (fDisplay f) (topBuf $ fBufs f) (gcForeground $ fGCs f)
		(map (uncurry Point) ps) nonconvex coordModeOrigin

---------------------------------------------------------------------------

fromTopLeft :: Field -> Double -> Double -> IO (Position, Position)
fromTopLeft f x y = do
	(width, height) <- fieldSize f
	return (round $ x + width / 2, round $ - y + height / 2)

fromCenter :: Field -> CInt -> CInt -> IO (Double, Double)
fromCenter f x y = do
	(width, height) <- fieldSize f
	return (fromIntegral x - width / 2, fromIntegral (- y) + height / 2)
