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
import Graphics.X11(
	closeDisplay, flush,
	copyArea, fillRectangle,
	allocaXEvent, nextEvent, XEventPtr,
	getGeometry, initThreads, connectionNumber, pending, destroyWindow,
	xK_VoidSymbol, buttonPress, buttonRelease
 )
import Graphics.X11.Xlib.Extras(Event(..), getEvent)
import Graphics.X11.Xim
import Graphics.X11.Turtle.FieldTools
import Graphics.X11.Turtle.Layers(redrawLayers)

import Data.Convertible(convert)
import Data.Maybe

import Control.Monad(forever, replicateM_, when)
import Control.Monad.Tools(doWhile_, whenM, unlessM)
import Control.Concurrent(
	forkIO, ThreadId, Chan, newChan, writeChan, readChan, threadWaitRead,
	killThread)

import System.Posix.Types

openField :: IO Field
openField = do
	(dpy, win, gc, gcBG, bufs@[undoBuf, bg, buf], ic, del, widthRef, heightRef)
		<- openWindow
	let size = do
		w <- readIORef widthRef
		h <- readIORef heightRef
		return (w, h)
	fll <- newLayers 50 
		(size >>= \(w, h) -> copyArea dpy undoBuf bg gc 0 0 w h 0 0)
		(size >>= \(w, h) -> fillRectangle dpy undoBuf gcBG 0 0 w h)
		(size >>= \(w, h) -> copyArea dpy bg buf gc 0 0 w h 0 0)
	f <- initialField dpy win gc gcBG del widthRef heightRef bufs fll
	_ <- forkIOX $ runLoop ic f
	flushWindow f
	return f

waitInput :: Field -> Chan () -> IO (Chan Bool)
waitInput f t = do
	c <- newChan
	tid <- forkIOX $ forever $ do
		putStrLn "before threadWaitRead"
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

makeInput :: Field -> XEventPtr -> Chan Bool -> Chan () -> IO ThreadId
makeInput f e endc t = do
	forkIOX $ forever $ do
		end <- readChan endc
		evN <- pending $ fDisplay f
		replicateM_ (fromIntegral evN) $ do
			putStrLn "before nextEvent"
			nextEvent (fDisplay f) e
			unlessM (filterEvent e 0) $ do
				ev <- getEvent e
				writeChan (fEvent f) $ Just ev
		when end $ writeChan (fEvent f) Nothing
		writeChan t ()

exposeFun :: Field -> IO Bool
exposeFun f = do
	(_, _, _, width, height, _, _) <- getGeometry (fDisplay f) (fWindow f)
	writeIORef (fWidth f) width
	writeIORef (fHeight f) height
	redrawLayers $ fLayers f
	flushWindow f
	return True

keyFun :: Field -> XIC -> XEventPtr -> IO Bool
keyFun f ic e = do
	(mstr, mks) <- utf8LookupString ic e
	let	str = fromMaybe " " mstr
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

runLoop :: XIC -> Field -> IO ()
runLoop ic f = allocaXEvent $ \e -> do
	timing <- newChan
	endc <- waitInput f timing
	th1 <- makeInput f e endc timing
	doWhile_ $ do
		mev <- readChan $ fEvent f
		case mev of
			Just (ExposeEvent{}) -> exposeFun f
			Just (KeyEvent{}) -> keyFun f ic e
			Just ev@ButtonEvent{} -> buttonFun f ev
			Just ev@MotionEvent{} -> motionFun f ev
			Just ev@ClientMessageEvent{} ->
				return $ convert (head $ ev_data ev) /= fDel f
			Nothing -> killThread th1 >> return False
			_ -> return True
	destroyWindow (fDisplay f) (fWindow f)
	closeDisplay $ fDisplay f
	writeChan (fEnd f) ()

forkIOX :: IO () -> IO ThreadId
forkIOX = (initThreads >>) . forkIO

flushWindow :: Field -> IO ()
flushWindow = withLock $ \f -> do
	(width, height) <- winSize f
	copyArea (fDisplay f) (fBuf f) (fWindow f) (fGC f) 0 0 width height 0 0
	flush $ fDisplay f
