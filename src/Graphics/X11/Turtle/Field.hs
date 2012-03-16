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
	fillPolygon,
	fillRectangle,
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
	Display, Window, Pixmap, Atom, Point(..), Dimension,
	XEventPtr, XIC, Bufs, undoBuf, bgBuf, topBuf,
	GCs, gcForeground, gcBackground, Event(..),
	forkIOX, openWindow, destroyWindow, closeDisplay, windowSize,
	flush, copyArea, setForegroundXT,
	drawLineXT, writeStringXT,
	allocaXEvent, waitEvent, pending, nextEvent, getEvent, filterEvent,
	utf8LookupString, buttonPress, buttonRelease, xK_VoidSymbol)
import qualified Graphics.X11.Turtle.XTools as X(fillPolygonXT, drawImageXT,
	fillRectangle, Position)
import Graphics.X11.Turtle.Layers(
	Layers, Layer, Character, newLayers, redrawLayers,
	makeLayer, addDraw, setBackground, undoLayer, clearLayer,
	makeCharacter, setCharacter)
import Text.XML.YJSVG(Position(..), Color(..))

import Control.Monad(forever, replicateM, when, join, unless)
import Control.Monad.Tools(doWhile_, doWhile, whenM)
import Control.Arrow((***))
import Control.Concurrent(
	forkIO, ThreadId, killThread, Chan, newChan, readChan, writeChan, threadDelay)

import Data.IORef(IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Maybe(fromMaybe)
import Data.Convertible(convert)

--------------------------------------------------------------------------------

data Field = Field{
	fDisplay :: Display, fWindow :: Window, fBufs :: Bufs, fGCs :: GCs,
	fIC :: XIC, fDel :: Atom, fSize :: IORef (Dimension, Dimension),

	fClick, fRelease :: IORef (Int -> Double -> Double -> IO Bool),
	fDrag :: IORef (Double -> Double -> IO ()),
	fMotion :: IORef (Double -> Double -> IO ()),
	fKeypress :: IORef (Char -> IO Bool), fPressed :: IORef Bool,
	fTimerEvent :: IORef (IO Bool),

	fLayers :: IORef Layers, fRunning :: IORef [ThreadId],
	fLock, fClose, fEnd :: Chan (),
	fInputChan :: Chan InputType
 }

makeField :: Display -> Window -> Bufs -> GCs -> XIC -> Atom ->
	IORef (Dimension, Dimension) -> IORef Layers -> IO Field
makeField dpy win bufs gcs ic del sizeRef ls = do
	[click, release] <- replicateM 2 $ newIORef $ \_ _ _ -> return True
	drag <- newIORef $ \_ _ -> return ()
	motion <- newIORef $ \_ _ -> return ()
	keypress <- newIORef $ \_ -> return True
	pressed <- newIORef False
	timer <- newIORef $ return True
	running <- newIORef []
	[lock, close, end] <- replicateM 3 newChan
	inputChan <- newChan
	writeChan lock ()
	return Field{
		fDisplay = dpy, fWindow = win, fBufs = bufs, fGCs = gcs,
		fIC = ic, fDel = del, fSize = sizeRef,

		fClick = click, fRelease = release, fDrag = drag, fMotion = motion,
		fKeypress = keypress, fPressed = pressed, fTimerEvent = timer,

		fLayers = ls,
		fRunning = running,
		fLock = lock, fClose = close, fEnd = end, fInputChan = inputChan
	 }

--------------------------------------------------------------------------------

openField :: IO Field
openField = do
	(dpy, win, bufs, gcs, ic, del, size) <- openWindow
	let	(ub, bb, tb) = (undoBuf bufs, bgBuf bufs, topBuf bufs)
		(gcf, gcb) = (gcForeground gcs, gcBackground gcs)
	sizeRef <- newIORef size
	let getSize = readIORef sizeRef
	ls <- newLayers 50 
		(getSize >>= uncurry (X.fillRectangle dpy ub gcb 0 0))
		(getSize >>= \(w, h) -> copyArea dpy ub bb gcf 0 0 w h 0 0)
		(getSize >>= \(w, h) -> copyArea dpy bb tb gcf 0 0 w h 0 0)
	f <- makeField dpy win bufs gcs ic del sizeRef ls
	_ <- forkIOX $ runLoop f
	flush dpy
	return f

data InputType = XInput | End | Timer

waitInput :: Field -> IO (Chan InputType, Chan ())
waitInput f = do
--	go <- newChan
	let	go = fInputChan f
	empty <- newChan
	tid <- forkIOX $ forever $ do
		waitEvent $ fDisplay f
		writeChan go XInput
		readChan empty
	modifyIORef (fRunning f) (tid :)
	_ <- forkIO $ do
		readChan $ fClose f
		killThread tid
		writeChan go End
	return (go, empty)

runLoop :: Field -> IO ()
runLoop f = allocaXEvent $ \e -> do
	(go, empty) <- waitInput f
	doWhile_ $ do
		iType <- readChan go
		cont' <- case iType of
			Timer -> do
				c <- join $ readIORef $ fTimerEvent f
				unless c $ readIORef (fRunning f) >>= mapM_ killThread
				return c -- readIORef (fTimerEvent f) >>= k
			_ -> return True
		let	notEnd = case iType of
				End -> False
				_ -> True
		cont <- doWhile True $ const $ do
			evN <- pending $ fDisplay f
			if evN > 0 then do
					nextEvent (fDisplay f) e
					filtered <- filterEvent e 0
					if filtered then return (True, True)
						else do	ev <- getEvent e
							c <- processEvent f e ev
							return (c, c)
				else return (True, False)
		if notEnd && cont && cont' then writeChan empty () >> return True
			else return False
	readIORef (fRunning f) >>= mapM_ killThread
	destroyWindow (fDisplay f) (fWindow f)
	closeDisplay $ fDisplay f
	writeChan (fEnd f) ()

processEvent :: Field -> XEventPtr -> Event -> IO Bool
processEvent f e ev = case ev of
	ExposeEvent{} -> flushField f True $ do
		windowSize (fDisplay f) (fWindow f) >>= writeIORef (fSize f)
		redrawLayers $ fLayers f
		return True
	KeyEvent{} -> do
		(mstr, mks) <- utf8LookupString (fIC f) e
		let	str = fromMaybe "" mstr
			_ks = fromMaybe xK_VoidSymbol mks
		readIORef (fKeypress f) >>= fmap and . ($ str) . mapM
	ButtonEvent{} -> do
		pos <- center (ev_x ev) (ev_y ev)
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
		pos <- center (ev_x ev) (ev_y ev)
		whenM (readIORef $ fPressed f) $
			readIORef (fDrag f) >>= ($ pos) . uncurry
		readIORef (fMotion f) >>= ($ pos) . uncurry
		return True
	ClientMessageEvent{} -> return $ convert (head $ ev_data ev) /= fDel f
	_ -> return True
	where
	center x y = do
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
	modifyIORef (fRunning f) (tid :)
	return tid

flushField :: Field -> Bool -> IO a -> IO a
flushField f real act = do
	readChan $ fLock f
	ret <- act
	when real $ do
		(w, h) <- readIORef $ fSize f
		copyArea (fDisplay f) (topBuf $ fBufs f) (fWindow f)
			(gcForeground $ fGCs f) 0 0 w h 0 0
		flush $ fDisplay f
	writeChan (fLock f) ()
	return ret

fieldColor :: Field -> Layer -> Color -> IO ()
fieldColor f l c = setBackground l $ do
	setForegroundXT (fDisplay f) (gcBackground $ fGCs f) c
	readIORef (fSize f) >>= uncurry (X.fillRectangle
		(fDisplay f) (undoBuf $ fBufs f) (gcBackground $ fGCs f) 0 0)

--------------------------------------------------------------------------------

addLayer :: Field -> IO Layer
addLayer = makeLayer . fLayers

drawLine :: Field -> Layer -> Double -> Color -> Position -> Position -> IO ()
--	Double -> Double -> Double -> Double -> IO ()
drawLine f l lw clr p1 p2 = do -- x1 y1 x2 y2 = do
	(x1, y1) <- getPosition f p1
	(x2, y2) <- getPosition f p2
	addDraw l (drawLineBuf f undoBuf (round lw) clr x1 y1 x2 y2,
		drawLineBuf f bgBuf (round lw) clr x1 y1 x2 y2)

writeString :: Field -> Layer -> String -> Double -> Color -> Position ->
	String -> IO ()
writeString f l fname size clr pos str = addDraw l (ws undoBuf, ws bgBuf)
	where ws bf = do
		(x, y) <- getPosition f pos
		writeStringXT (fDisplay f) (bf $ fBufs f) fname size clr x y str

drawImage :: Field -> Layer -> FilePath -> Position -> Double -> Double -> IO ()
drawImage f l fp pos w h = addDraw l (di undoBuf, di bgBuf)
	where di bf = do
		(x, y) <- getPosition f pos
		X.drawImageXT (fDisplay f) (bf $ fBufs f) (gcForeground $ fGCs f)
			fp x y (round w) (round h)

fillPolygon :: Field -> Layer -> [Position] -> Color -> IO ()
fillPolygon f l poss clr = addDraw l (fp undoBuf, fp bgBuf)
	where fp bf = do
		let psc = map (\Center{posX = x, posY = y} -> (x, y)) poss
		ps <- mapM (fmap (uncurry Point) . uncurry (topLeft f)) psc
		setForegroundXT (fDisplay f) (gcForeground $ fGCs f) clr
		X.fillPolygonXT (fDisplay f) (bf $ fBufs f) (gcForeground $ fGCs f) ps

fillRectangle :: Field -> Layer -> Position -> Double -> Double -> Color -> IO ()
fillRectangle f l p w h clr = addDraw l (fr undoBuf, fr bgBuf)
	where fr bf = do
--		(x0, y0) <- topLeft f xc0 yc0
		(x0, y0) <- getPosition f p
		setForegroundXT (fDisplay f) (gcForeground $ fGCs f) clr
		X.fillRectangle (fDisplay f) (bf $ fBufs f) (gcForeground $ fGCs f)
			x0 y0 (round w) (round h)

drawLineBuf :: Field -> (Bufs -> Pixmap) -> Int -> Color ->
	X.Position -> X.Position -> X.Position -> X.Position -> IO ()
drawLineBuf f bf lw c x1 y1 x2 y2 = do
	drawLineXT (fDisplay f) (gcForeground $ fGCs f) (bf $ fBufs f) lw c
		x1 y1 x2 y2

getPosition :: Field -> Position -> IO (X.Position, X.Position)
getPosition f (Center x y) = topLeft f x y
getPosition _ (TopLeft x y) = return (round x, round y)

topLeft :: Field -> Double -> Double -> IO (X.Position, X.Position)
topLeft f x y = do
	(width, height) <- fieldSize f
	return (round x + round (width / 2), - round y + round (height / 2))

--------------------------------------------------------------------------------

addCharacter :: Field -> IO Character
addCharacter = makeCharacter . fLayers

drawCharacter :: Field -> Character -> Color -> [Position] -> IO ()
drawCharacter f c clr sh = setCharacter c $ drawShape f clr sh

drawCharacterAndLine ::	Field -> Character -> Color -> [Position] ->
	Double -> Position -> Position -> IO () -- Double -> Double -> Double -> Double -> IO ()
drawCharacterAndLine f c clr sh lw p1 p2 = do
	(x1, y1) <- getPosition f p1
	(x2, y2) <- getPosition f p2
	setCharacter c $
		drawShape f clr sh >> drawLineBuf f topBuf (round lw) clr x1 y1 x2 y2

drawShape :: Field -> Color -> [Position] -> IO ()
drawShape f clr psc = do
	ps <- mapM (fmap (uncurry Point) . getPosition f) psc -- uncurry (topLeft f)) psc
	setForegroundXT (fDisplay f) (gcForeground $ fGCs f) clr
	X.fillPolygonXT (fDisplay f) (topBuf $ fBufs f) (gcForeground $ fGCs f) ps

clearCharacter :: Character -> IO ()
clearCharacter c = setCharacter c $ return ()

--------------------------------------------------------------------------------

onclick, onrelease :: Field -> (Int -> Double -> Double -> IO Bool) -> IO ()
(onclick, onrelease) = (writeIORef .) *** (writeIORef .) $ (fClick, fRelease)

ondrag, onmotion :: Field -> (Double -> Double -> IO ()) -> IO ()
ondrag = writeIORef . fDrag
onmotion = writeIORef . fMotion

onkeypress :: Field -> (Char -> IO Bool) -> IO ()
onkeypress = writeIORef . fKeypress

ontimer :: Field -> Int -> IO Bool -> IO ()
ontimer f t fun = do
	writeIORef (fTimerEvent f) fun
	threadDelay $ t * 1000
	writeChan (fInputChan f) Timer
