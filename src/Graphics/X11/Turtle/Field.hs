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
	XEventPtr, Event(..), XIC, Bufs, undoBuf, bgBuf, topBuf,
	GCs, gcForeground, gcBackground,
	forkIOX, openWindow, destroyWindow, closeDisplay, windowSize,
	flush, setForegroundXT, copyAreaXT,
	drawLineXT, fillRectangleXT, fillPolygonXT, writeStringXT, drawImageXT,
	allocaXEvent, waitEvent, pending, nextEvent, getEvent, filterEvent,
	utf8LookupString, buttonPress, buttonRelease, xK_VoidSymbol)
import Graphics.X11.Turtle.Layers(
	Layers, Layer, Character, newLayers, redrawLayers,
	makeLayer, background, addDraw, undoLayer, clearLayer,
	makeCharacter, character)
import Text.XML.YJSVG(Position(..), Color(..))

import Control.Monad(when, unless, forever, replicateM, forM_, join)
import Control.Monad.Tools(doWhile_, doWhile)
import Control.Arrow((***))
import Control.Concurrent(
	ThreadId, forkIO, killThread, threadDelay,
	Chan, newChan, readChan, writeChan)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.IORef.Tools(atomicModifyIORef_)
import Data.Maybe(fromMaybe)
import Data.List(delete)
import Data.Convertible(convert)
import Data.Function.Tools(const2, const3)

--------------------------------------------------------------------------------

data Field = Field{
	fDisplay :: Display, fWindow :: Window, fBufs :: Bufs, fGCs :: GCs,
	fIC :: XIC, fDel :: Atom, fSize :: IORef (Dimension, Dimension),

	fClick, fRelease :: IORef (Int -> Double -> Double -> IO Bool),
	fDrag :: IORef (Int -> Double -> Double -> IO ()),
	fPressed :: IORef [Int],
	fMotion :: IORef (Double -> Double -> IO ()),
	fKeypress :: IORef (Char -> IO Bool), fTimerEvent :: IORef (IO Bool),

	fLayers :: IORef Layers, fCoordinates :: IORef Coordinates,
	fInput :: Chan InputType, fLock, fClose, fEnd :: Chan (),
	fRunning :: IORef [ThreadId]}

makeField :: Display -> Window -> Bufs -> GCs -> XIC -> Atom ->
	IORef (Dimension, Dimension) -> IORef Layers -> IO Field
makeField dpy win bufs gcs ic del sizeRef ls = do
	[click, release] <- replicateM 2 $ newIORef $ const3 $ return True
	drag <- newIORef $ const3 $ return ()
	pressed <- newIORef []
	motion <- newIORef $ const2 $ return ()
	keypress <- newIORef $ const $ return True
	timer <- newIORef $ return True
	input <- newChan
	coord <- newIORef CoordCenter
	[lock, close, end] <- replicateM 3 newChan
	running <- newIORef []
	writeChan lock ()
	return Field{
		fDisplay = dpy, fWindow = win, fBufs = bufs, fGCs = gcs,
		fIC = ic, fDel = del, fSize = sizeRef,

		fClick = click, fRelease = release,
		fDrag = drag, fPressed = pressed,
		fMotion = motion, fKeypress = keypress, fTimerEvent = timer,

		fLayers = ls, fCoordinates = coord, fInput = input,
		fLock = lock, fClose = close, fEnd = end, fRunning = running}

setPressed :: Field -> Int -> Bool -> IO ()
setPressed f buttonN True = atomicModifyIORef_ (fPressed f) (buttonN :)
setPressed f buttonN False = atomicModifyIORef_ (fPressed f) (delete buttonN)

killRunning :: Field -> IO ()
killRunning f = readIORef (fRunning f) >>= mapM_ killThread

data Coordinates = CoordCenter | CoordTopLeft

--------------------------------------------------------------------------------

openField :: IO Field
openField = do
	(dpy, win, bufs, gcs, ic, del, size) <- openWindow
	sizeRef <- newIORef size
	let	(ub, bb, tb) = (undoBuf bufs, bgBuf bufs, topBuf bufs)
		(gcf, gcb) = (gcForeground gcs, gcBackground gcs)
	lyrs <- newLayers 50 (setForegroundXT dpy gcb (RGB 255 255 255) >>
		readIORef sizeRef >>= uncurry (fillRectangleXT dpy ub gcb 0 0))
		(readIORef sizeRef >>= uncurry (copyAreaXT dpy ub bb gcf))
		(readIORef sizeRef >>= uncurry (copyAreaXT dpy bb tb gcf))
	f <- makeField dpy win bufs gcs ic del sizeRef lyrs
	_ <- forkIOX $ runLoop f
	flush dpy
	return f

data InputType = XInput | End | Timer

waitInput :: Field -> IO (Chan ())
waitInput f = do
	empty <- newChan
	tid <- forkIOX $ forever $ do
		waitEvent (fDisplay f) >> writeChan (fInput f) XInput
		readChan empty
	atomicModifyIORef_ (fRunning f) (tid :)
	_ <- forkIO $
		readChan (fClose f) >> killRunning f >> writeChan (fInput f) End
	return empty

runLoop :: Field -> IO ()
runLoop f = allocaXEvent $ \e -> do
	empty <- waitInput f
	doWhile_ $ do
		iType <- readChan $ fInput f
		case iType of
			End -> return False
			Timer -> do
				cont <- join $ readIORef $ fTimerEvent f
				unless cont $ killRunning f
				return cont
			XInput -> do
				cont <- processXInput f e
				when cont $ writeChan empty ()
				return cont
	destroyWindow (fDisplay f) (fWindow f)
	closeDisplay $ fDisplay f
	writeChan (fEnd f) ()

processXInput :: Field -> XEventPtr -> IO Bool
processXInput f e = doWhile undefined $ const $ do
	evN <- pending $ fDisplay f
	if evN <= 0 then return (True, False) else do
		nextEvent (fDisplay f) e
		filtered <- filterEvent e 0
		if filtered then return (undefined, True) else do
			ev <- getEvent e
			c <- processEvent f e ev
			unless c $ killRunning f
			return (c && undefined, c)

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
		let buttonN = fromIntegral $ ev_button ev
		pos <- getEventXY f ev
		case ev_event_type ev of
			et	| et == buttonPress -> do
					setPressed f buttonN True
					readIORef (fClick f) >>=
						($ pos) . uncurry . ($ buttonN)
				| et == buttonRelease -> do
					setPressed f buttonN False
					readIORef (fRelease f) >>=
						($ pos) . uncurry . ($ buttonN)
			_ -> error "not implement event"
	MotionEvent{} -> do
		pos <- getEventXY f ev
		pressed <- readIORef $ fPressed f
		forM_ pressed $ \bn -> readIORef (fDrag f) >>=
			($ pos) . uncurry . ($ bn)
		readIORef (fMotion f) >>= ($ pos) . uncurry
		return True
	ClientMessageEvent{} -> return $ convert (head $ ev_data ev) /= fDel f
	_ -> return True

getEventXY :: Field -> Event -> IO (Double, Double)
getEventXY f ev = do
	let [x, y] = map (fromIntegral . ($ ev)) [ev_x, ev_y]
	coord <- readIORef $ fCoordinates f
	case coord of
		CoordCenter -> do
			(w, h) <- fieldSize f
			return (x - w / 2, h / 2 - y)
		CoordTopLeft -> return (x, y)

closeField :: Field -> IO ()
closeField = flip writeChan () . fClose

waitField :: Field -> IO ()
waitField = readChan . fEnd

topleft, center :: Field -> IO ()
topleft = flip (writeIORef . fCoordinates) CoordTopLeft
center = flip (writeIORef . fCoordinates) CoordCenter

coordinates :: Field -> IO Coordinates
coordinates = readIORef . fCoordinates

fieldSize :: Field -> IO (Double, Double)
fieldSize = fmap (fromIntegral *** fromIntegral) . readIORef . fSize

--------------------------------------------------------------------------------

forkField :: Field -> IO () -> IO ThreadId
forkField f act = do
	tid <- forkIOX act
	atomicModifyIORef_ (fRunning f) (tid :) >> return tid

flushField :: Field -> Bool -> IO a -> IO a
flushField f@Field{fDisplay = dpy, fWindow = win} real act = do
	ret <- readChan (fLock f) >> act
	when real $ do
		let (tb, gc) = (topBuf $ fBufs f, gcForeground $ fGCs f)
		uncurry (copyAreaXT dpy tb win gc) =<< readIORef (fSize f)
		flush $ fDisplay f
	writeChan (fLock f) () >> return ret

fieldColor :: Field -> Layer -> Color -> IO ()
fieldColor f@Field{fDisplay = dpy} l clr = background l $ do
	let (ub, gc) = (undoBuf $ fBufs f, gcBackground $ fGCs f)
	setForegroundXT dpy gc clr
	uncurry (fillRectangleXT dpy ub gc 0 0) =<< readIORef (fSize f)

--------------------------------------------------------------------------------

addLayer :: Field -> IO Layer
addLayer = makeLayer . fLayers

drawLayer :: Field -> Layer -> (Pixmap -> IO ()) -> IO ()
drawLayer Field{fBufs = bs} l drw = addDraw l (drw $ undoBuf bs, drw $ bgBuf bs)

drawLine :: Field -> Layer -> Double -> Color -> Position -> Position -> IO ()
drawLine f l w c p q = drawLayer f l $ \buf -> drawLineBuf f buf (round w) c p q

drawLineBuf :: Field -> Pixmap -> Int -> Color -> Position -> Position -> IO ()
drawLineBuf f@Field{fDisplay = dpy} buf lw clr p q = do
	(x1, y1) <- getPosition f p
	(x2, y2) <- getPosition f q
	drawLineXT dpy (gcForeground $ fGCs f) buf lw clr x1 y1 x2 y2

writeString :: Field -> Layer -> String -> Double -> Color -> Position ->
	String -> IO ()
writeString f@Field{fDisplay = dpy} l fname size clr pos str =
	drawLayer f l $ \buf -> getPosition f pos >>=
		flip (uncurry $ writeStringXT dpy buf fname size clr) str

drawImage :: Field -> Layer -> FilePath -> Position -> Double -> Double -> IO ()
drawImage f@Field{fDisplay = dpy} l fp pos w h = drawLayer f l $ \buf -> do
	(x, y) <- getPosition f pos
	drawImageXT dpy buf (gcForeground $ fGCs f) fp x y (round w) (round h)

fillRectangle :: Field -> Layer -> Position -> Double -> Double -> Color -> IO ()
fillRectangle f@Field{fDisplay = dpy} l p w h clr = drawLayer f l $ \buf -> do
	(x, y) <- getPosition f p
	setForegroundXT dpy (gcForeground $ fGCs f) clr
	fillRectangleXT dpy buf (gcForeground $ fGCs f) x y (round w) (round h)

fillPolygon :: Field -> Layer -> [Position] -> Color -> IO ()
fillPolygon f l ps clr = drawLayer f l $ \buf -> fillPolygonBuf f buf clr ps

fillPolygonBuf :: Field -> Pixmap -> Color -> [Position] -> IO ()
fillPolygonBuf f@Field{fDisplay = dpy} buf clr positions = do
	ps <- mapM (fmap (uncurry Point) . getPosition f) positions
	setForegroundXT dpy (gcForeground $ fGCs f) clr
	fillPolygonXT dpy buf (gcForeground $ fGCs f) ps

getPosition :: Field -> Position -> IO (PositionXT, PositionXT)
getPosition f (Center x y) = do
	(w, h) <- fieldSize f
	return (round x + round (w / 2), - round y + round (h / 2))
getPosition _ (TopLeft x y) = return (round x, round y)

--------------------------------------------------------------------------------

addCharacter :: Field -> IO Character
addCharacter = makeCharacter . fLayers

drawCharacter :: Field -> Character -> Color -> [Position] -> IO ()
drawCharacter f ch c = character ch . fillPolygonBuf f (topBuf $ fBufs f) c

drawCharacterAndLine ::	Field -> Character -> Color -> [Position] ->
	Double -> Position -> Position -> IO ()
drawCharacterAndLine f ch clr sh lw p q = character ch $ do
	fillPolygonBuf f (topBuf $ fBufs f) clr sh
	drawLineBuf f (topBuf $ fBufs f) (round lw) clr p q

clearCharacter :: Character -> IO ()
clearCharacter ch = character ch $ return ()

--------------------------------------------------------------------------------

onclick, onrelease :: Field -> (Int -> Double -> Double -> IO Bool) -> IO ()
[onclick, onrelease] = map (writeIORef .) [fClick, fRelease]

ondrag :: Field -> (Int -> Double -> Double -> IO ()) -> IO ()
ondrag = writeIORef . fDrag

onmotion :: Field -> (Double -> Double -> IO ()) -> IO ()
onmotion = writeIORef . fMotion

onkeypress :: Field -> (Char -> IO Bool) -> IO ()
onkeypress = writeIORef . fKeypress

ontimer :: Field -> Int -> IO Bool -> IO ()
ontimer f t fun = do
	writeIORef (fTimerEvent f) fun
	_ <- forkIO $ threadDelay (t * 1000) >> writeChan (fInput f) Timer
	return ()
