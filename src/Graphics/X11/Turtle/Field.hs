module Graphics.X11.Turtle.Field(
	-- * types and classes
	Field,
	Layer,
	Character,
	Coordinates(..),

	-- * basic functions
	openField,
	closeField,
	waitField,
	topleft,
	center,
	coordinates,
	fieldSize,

	-- * draw
	forkField,
	flushField,
	fieldColor,

	-- ** to Layer
	addLayer,
	drawLine,
	fillRectangle,
	fillPolygon,
	writeString,
	drawImage,
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
	onmotion,
	onkeypress,
	ontimer
) where

import Graphics.X11.Turtle.XTools(
	Display, Window, Pixmap, Atom, Point(..), PositionXT, Dimension,
	XEventPtr, XIC, Bufs, undoBuf, bgBuf, topBuf,
	GCs, gcForeground, gcBackground, Event(..),
	forkIOX, openWindow, destroyWindow, closeDisplay, windowSize,
	flush, copyArea, setForegroundXT,
	drawLineXT, fillRectangleXT, fillPolygonXT, writeStringXT, drawImageXT,
	allocaXEvent, waitEvent, pending, nextEvent, getEvent, filterEvent,
	utf8LookupString, buttonPress, buttonRelease, xK_VoidSymbol)
import Graphics.X11.Turtle.Layers(
	Layers, Layer, Character, newLayers, redrawLayers,
	makeLayer, background, addDraw, undoLayer, clearLayer,
	makeCharacter, character)
import Text.XML.YJSVG(Position(..), Color(..))

import Control.Monad(when, unless, forever, replicateM, join)
import Control.Monad.Tools(doWhile_, doWhile, whenM)
import Control.Arrow((***))
import Control.Concurrent(
	ThreadId, forkIO, killThread, threadDelay,
	Chan, newChan, readChan, writeChan)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.IORef.Tools(atomicModifyIORef_)
import Data.Maybe(fromMaybe)
import Data.Convertible(convert)

--------------------------------------------------------------------------------

data Field = Field{
	fDisplay :: Display, fWindow :: Window, fBufs :: Bufs, fGCs :: GCs,
	fIC :: XIC, fDel :: Atom, fSize :: IORef (Dimension, Dimension),

	fClick, fRelease :: IORef (Int -> Double -> Double -> IO Bool),
	fDrag, fMotion :: IORef (Double -> Double -> IO ()),
	fKeypress :: IORef (Char -> IO Bool),
	fTimerEvent :: IORef (IO Bool),
	fPressed :: IORef Bool,

	fLayers :: IORef Layers, fInput :: Chan InputType,
	fCoordinates :: IORef Coordinates, fLock, fClose, fEnd :: Chan (),
	fRunning :: IORef [ThreadId]}

makeField :: Display -> Window -> Bufs -> GCs -> XIC -> Atom ->
	IORef (Dimension, Dimension) -> IORef Layers -> IO Field
makeField dpy win bufs gcs ic del sizeRef ls = do
	[click, release] <- replicateM 2 $ newIORef $ \_ _ _ -> return True
	[drag, motion] <- replicateM 2 $ newIORef $ \_ _ -> return ()
	keypress <- newIORef $ \_ -> return True
	timer <- newIORef $ return True
	pressed <- newIORef False
	input <- newChan
	coord <- newIORef CoordCenter
	[lock, close, end] <- replicateM 3 newChan
	running <- newIORef []
	writeChan lock ()
	return Field{
		fDisplay = dpy, fWindow = win, fBufs = bufs, fGCs = gcs,
		fIC = ic, fDel = del, fSize = sizeRef,

		fClick = click, fRelease = release, fDrag = drag,
		fMotion = motion, fKeypress = keypress, fTimerEvent = timer,
		fPressed = pressed,

		fLayers = ls, fInput = input, fCoordinates = coord,
		fLock = lock, fClose = close, fEnd = end, fRunning = running}

data Coordinates = CoordCenter | CoordTopLeft

coordinates :: Field -> IO Coordinates
coordinates = readIORef . fCoordinates

topleft, center :: Field -> IO ()
topleft = flip (writeIORef . fCoordinates) CoordTopLeft
center = flip (writeIORef . fCoordinates) CoordCenter

--------------------------------------------------------------------------------

openField :: IO Field
openField = do
	(dpy, win, bufs, gcs, ic, del, size) <- openWindow
	let	(ub, bb, tb) = (undoBuf bufs, bgBuf bufs, topBuf bufs)
		(gcf, gcb) = (gcForeground gcs, gcBackground gcs)
	sizeRef <- newIORef size
	let	getSize = readIORef sizeRef
	ls <- newLayers 50 
		(setForegroundXT dpy gcb (RGB 255 255 255) >>
			getSize >>= uncurry (fillRectangleXT dpy ub gcb 0 0))
		(getSize >>= \(w, h) -> copyArea dpy ub bb gcf 0 0 w h 0 0)
		(getSize >>= \(w, h) -> copyArea dpy bb tb gcf 0 0 w h 0 0)
	f <- makeField dpy win bufs gcs ic del sizeRef ls
	_ <- forkIOX $ runLoop f
	flush dpy
	return f

data InputType = XInput | End | Timer

waitInput :: Field -> IO (Chan ())
waitInput f = do
	empty <- newChan
	tid <- forkIOX $ forever $ do
		waitEvent $ fDisplay f
		writeChan (fInput f) XInput
		readChan empty
	atomicModifyIORef_ (fRunning f) (tid :)
	_ <- forkIO $ do
		readChan $ fClose f
		killThread tid
		writeChan (fInput f) End
	return empty

runLoop :: Field -> IO ()
runLoop f = allocaXEvent $ \e -> do
	empty <- waitInput f
	doWhile_ $ do
		iType <- readChan $ fInput f
		cont <- case iType of
			End -> return False
			Timer -> do
				c <- join $ readIORef $ fTimerEvent f
				unless c $ readIORef (fRunning f)
					>>= mapM_ killThread
				return c
			XInput -> doWhile undefined $ const $ do
				evN <- pending $ fDisplay f
				if evN <= 0 then return (True, False) else do
					nextEvent (fDisplay f) e
					filtered <- filterEvent e 0
					if filtered then return (True, True)
						else do	ev <- getEvent e
							c <- processEvent f e ev
							return (c, c)
		if cont then writeChan empty () >> return True else return False
	readIORef (fRunning f) >>= mapM_ killThread
	destroyWindow (fDisplay f) (fWindow f)
	closeDisplay $ fDisplay f
	writeChan (fEnd f) ()

processEvent :: Field -> XEventPtr -> Event -> IO Bool
processEvent f e ev = case ev of
	ExposeEvent{} -> flushField f True $ do
		windowSize (fDisplay f) (fWindow f) >>= writeIORef (fSize f)
		redrawLayers (fLayers f) >> return True
	KeyEvent{} -> do
		(mstr, mks) <- utf8LookupString (fIC f) e
		let	str = fromMaybe "" mstr
			_ks = fromMaybe xK_VoidSymbol mks
		readIORef (fKeypress f) >>= fmap and . ($ str) . mapM
	ButtonEvent{} -> do
		coord <- readIORef $ fCoordinates f
		pos <- case coord of
			CoordCenter -> cntr (ev_x ev) (ev_y ev)
			CoordTopLeft -> return
				(fromIntegral $ ev_x ev, fromIntegral $ ev_y ev)
		let	buttonN = fromIntegral $ ev_button ev
		case ev_event_type ev of
			et	| et == buttonPress -> do
					writeIORef (fPressed f) True
					readIORef (fClick f) >>=
						($ pos) . uncurry . ($ buttonN)
				| et == buttonRelease -> do
					writeIORef (fPressed f) False
					readIORef (fRelease f) >>=
						($ pos) . uncurry . ($ buttonN)
			_ -> error "not implement event"
	MotionEvent{} -> do
		pos <- cntr (ev_x ev) (ev_y ev)
		whenM (readIORef $ fPressed f) $
			readIORef (fDrag f) >>= ($ pos) . uncurry
		readIORef (fMotion f) >>= ($ pos) . uncurry
		return True
	ClientMessageEvent{} -> return $ convert (head $ ev_data ev) /= fDel f
	_ -> return True
	where
	cntr x y = do
		(w, h) <- fieldSize f
		return (fromIntegral x - w / 2, fromIntegral (- y) + h / 2)

closeField :: Field -> IO ()
closeField = flip writeChan () . fClose

waitField :: Field -> IO ()
waitField = readChan . fEnd

fieldSize :: Field -> IO (Double, Double)
fieldSize = fmap (fromIntegral *** fromIntegral) . readIORef . fSize

--------------------------------------------------------------------------------

forkField :: Field -> IO () -> IO ThreadId
forkField f act = do
	tid <- forkIOX act
	atomicModifyIORef_ (fRunning f) (tid :) >> return tid

flushField :: Field -> Bool -> IO a -> IO a
flushField f real act = do
	ret <- readChan (fLock f) >> act
	when real $ do
		(w, h) <- readIORef $ fSize f
		copyArea (fDisplay f) (topBuf $ fBufs f) (fWindow f)
			(gcForeground $ fGCs f) 0 0 w h 0 0
		flush $ fDisplay f
	writeChan (fLock f) () >> return ret

fieldColor :: Field -> Layer -> Color -> IO ()
fieldColor f l clr = background l $ do
	setForegroundXT (fDisplay f) (gcBackground $ fGCs f) clr
	readIORef (fSize f) >>= uncurry (fillRectangleXT
		(fDisplay f) (undoBuf $ fBufs f) (gcBackground $ fGCs f) 0 0)

--------------------------------------------------------------------------------

addLayer :: Field -> IO Layer
addLayer = makeLayer . fLayers

drawLayer :: Field -> Layer -> (Pixmap -> IO ()) -> IO ()
drawLayer Field{fBufs = bufs} l draw =
	addDraw l (draw $ undoBuf bufs, draw $ bgBuf bufs)

drawLine :: Field -> Layer -> Double -> Color -> Position -> Position -> IO ()
drawLine f l lw clr p1 p2 =
	drawLayer f l $ \buf -> drawLineBuf f buf (round lw) clr p1 p2

writeString :: Field -> Layer -> String -> Double -> Color -> Position ->
	String -> IO ()
writeString f@Field{fDisplay = dpy} l fname size clr pos str =
	drawLayer f l $ \buf -> getPosition f pos >>=
		flip (uncurry $ writeStringXT dpy buf fname size clr) str

drawImage :: Field -> Layer -> FilePath -> Position -> Double -> Double -> IO ()
drawImage f@Field{fDisplay = dpy} l fp pos w h = drawLayer f l $ \buf -> do
	(x, y) <- getPosition f pos
	drawImageXT dpy buf (gcForeground $ fGCs f) fp x y (round w) (round h)

fillPolygon :: Field -> Layer -> [Position] -> Color -> IO ()
fillPolygon f@Field{fDisplay = dpy} l positions clr = drawLayer f l $ \buf -> do
	ps <- mapM (fmap (uncurry Point) . getPosition f) positions
	setForegroundXT dpy (gcForeground $ fGCs f) clr
	fillPolygonXT dpy buf (gcForeground $ fGCs f) ps

fillRectangle :: Field -> Layer -> Position -> Double -> Double -> Color -> IO ()
fillRectangle f@Field{fDisplay = dpy} l p w h clr = drawLayer f l $ \buf -> do
	(x, y) <- getPosition f p
	setForegroundXT dpy (gcForeground $ fGCs f) clr
	fillRectangleXT dpy buf (gcForeground $ fGCs f) x y (round w) (round h)

drawLineBuf :: Field -> Pixmap -> Int -> Color -> Position -> Position -> IO ()
drawLineBuf f@Field{fDisplay = dpy} buf lw clr p1 p2 = do
	(x1, y1) <- getPosition f p1
	(x2, y2) <- getPosition f p2
	drawLineXT dpy (gcForeground $ fGCs f) buf lw clr x1 y1 x2 y2

getPosition :: Field -> Position -> IO (PositionXT, PositionXT)
getPosition f (Center x y) = do
	(w, h) <- fieldSize f
	return (round x + round (w / 2), - round y + round (h / 2))
getPosition _ (TopLeft x y) = return (round x, round y)

--------------------------------------------------------------------------------

addCharacter :: Field -> IO Character
addCharacter = makeCharacter . fLayers

drawCharacter :: Field -> Character -> Color -> [Position] -> IO ()
drawCharacter f c clr sh = character c $ drawShape f clr sh

drawCharacterAndLine ::	Field -> Character -> Color -> [Position] ->
	Double -> Position -> Position -> IO ()
drawCharacterAndLine f c clr sh lw p1 p2 = character c $ do
	drawShape f clr sh
	drawLineBuf f (topBuf $ fBufs f) (round lw) clr p1 p2

drawShape :: Field -> Color -> [Position] -> IO ()
drawShape f clr positions = do
	ps <- mapM (fmap (uncurry Point) . getPosition f) positions
	setForegroundXT (fDisplay f) (gcForeground $ fGCs f) clr
	fillPolygonXT (fDisplay f) (topBuf $ fBufs f) (gcForeground $ fGCs f) ps

clearCharacter :: Character -> IO ()
clearCharacter c = character c $ return ()

--------------------------------------------------------------------------------

onclick, onrelease :: Field -> (Int -> Double -> Double -> IO Bool) -> IO ()
(onclick, onrelease) = (writeIORef . fClick, writeIORef . fRelease)

ondrag, onmotion :: Field -> (Double -> Double -> IO ()) -> IO ()
(ondrag, onmotion) = (writeIORef . fDrag, writeIORef . fMotion)

onkeypress :: Field -> (Char -> IO Bool) -> IO ()
onkeypress = writeIORef . fKeypress

ontimer :: Field -> Int -> IO Bool -> IO ()
ontimer f t fun = do
	writeIORef (fTimerEvent f) fun
	threadDelay $ t * 1000
	writeChan (fInput f) Timer
