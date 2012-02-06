module Graphics.X11.WindowLayers(
	Field,
	Layer,
	Character,

	openField,
	closeField,
	layerSize,

	addLayer,
	addCharacter,

	drawLine,
	drawCharacter,
	drawCharacterAndLine,
	clearCharacter,

	undoLayer,
	clearLayer,
	flushLayer,

	forkIOX,
	addThread
) where

import Graphics.X11(
	Display, Window, Pixmap, Atom, GC, Point(..), Dimension, Position, Pixel,

	openDisplay, closeDisplay, flush, defaultScreen, rootWindow,
	whitePixel, blackPixel,	defaultDepth,
	createSimpleWindow, mapWindow, createPixmap, internAtom, createGC,

	setForeground, copyArea,
	fillRectangle, fillPolygon, nonconvex, coordModeOrigin,

	setWMProtocols, selectInput, allocaXEvent, nextEvent,
	keyPressMask, exposureMask,

	getGeometry, initThreads, connectionNumber, pending, destroyWindow
 )
import qualified Graphics.X11 as X (drawLine)
import Graphics.X11.Xlib.Extras(Event(..), getEvent)

import Data.IORef(IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Bits((.|.))
import Data.Convertible(convert)
import Data.List.Tools(modifyAt, setAt)
import Data.Bool.Tools(whether)

import Control.Monad(replicateM, forM_, forever, replicateM_, when)
import Control.Monad.Tools(doWhile_)
import Control.Arrow((***))
import Control.Concurrent(
	forkIO, ThreadId, Chan, newChan, writeChan, readChan, threadWaitRead,
	killThread, threadDelay)

import System.Posix.Types

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
	fBuffed :: IORef [IO ()],
	fLayers :: IORef [[Bool -> IO ()]],
	fCharacters :: IORef [IO ()],
	fWait :: Chan (),
	fEvent :: Chan (Maybe Event),
	fClose :: Chan (),
	fClosed :: IORef Bool,
	fRunning :: IORef [ThreadId]
 }

data Layer = Layer{
	layerField :: Field,
	layerId :: Int
 }

data Character = Character{
	characterField :: Field,
	characterId :: Int
 }

openField :: IO Field
openField = do
	_ <- initThreads
	dpy <- openDisplay ""
	del <- internAtom dpy "WM_DELETE_WINDOW" True
	let	scr = defaultScreen dpy
	root <- rootWindow dpy scr
	(_, _, _, rWidth, rHeight, _, _) <- getGeometry dpy root
	let	black = blackPixel dpy scr
		white = whitePixel dpy scr
		depth = defaultDepth dpy scr
	bufs <- replicateM 3 $ createPixmap dpy root rWidth rHeight depth
	win <- createSimpleWindow dpy root 0 0 rWidth rHeight 1 black white
	[gc, gcBG] <- replicateM 2 $ createGC dpy win
	setForeground dpy gcBG 0xffffff
	forM_ bufs $ \bf -> fillRectangle dpy bf gcBG 0 0 rWidth rHeight
	setWMProtocols dpy win [del]
	selectInput dpy win $ exposureMask .|. keyPressMask
	mapWindow dpy win
	[widthRef, heightRef] <- mapM newIORef [rWidth, rHeight]
	buffActions <- newIORef []
	layerActions <- newIORef []
	characterActions <- newIORef []
	wait <- newChan
	event <- newChan
	close <- newChan
	closed <- newIORef False
	running <- newIORef []
	writeChan wait ()
	let f = Field{
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
		fBuffed = buffActions,
		fLayers = layerActions,
		fCharacters = characterActions,
		fWait = wait,
		fEvent = event,
		fClose = close,
		fClosed = closed,
		fRunning = running
	 }
	_ <- forkIOX $ runLoop f
	flushWindow f
	return f

runLoop :: Field -> IO ()
runLoop f = allocaXEvent $ \e -> do
	endc <- waitInput f
	th1 <- forkIOX $ forever $ do
		evN <- pending $ fDisplay f
		replicateM_ (fromIntegral evN) $ do
			nextEvent (fDisplay f) e
			ev <- getEvent e
			writeChan (fEvent f) $ Just ev
		end <- readChan endc
		when end $ writeChan (fEvent f) Nothing
	(>> closeDisplay (fDisplay f)) $
		(>> destroyWindow (fDisplay f) (fWindow f)) $ doWhile_ $ do
		mev <- readChan $ fEvent f
		case mev of
			Just (ExposeEvent{}) -> do
				(_, _, _, width, height, _, _) <-
					getGeometry (fDisplay f) (fWindow f)
				writeIORef (fWidth f) width
				writeIORef (fHeight f) height
				redrawAll f
				return True
			Just (KeyEvent{}) -> return True
			Just ev@(ClientMessageEvent{}) ->
				return $ convert (head $ ev_data ev) /= fDel f
			Nothing -> killThread th1 >> return False
			_ -> return True

getConnection :: Field -> Fd
getConnection = Fd . connectionNumber . fDisplay

waitInput :: Field -> IO (Chan Bool)
waitInput f = do
	c <- newChan
	_ <- forkIOX $ forever $ do
		threadWaitRead $ getConnection f
		writeChan c False
	_ <- forkIO $ do
		readChan $ fClose f
		writeChan c True
	return c

closeField :: Field -> IO ()
closeField f = do
	readIORef (fRunning f) >>= mapM_ killThread
	threadDelay 100000
	writeChan (fClose f) ()
	writeIORef (fClosed f) True

addThread :: Field -> ThreadId -> IO ()
addThread f tid = modifyIORef (fRunning f) (tid :)

layerSize :: Layer -> IO (Double, Double)
layerSize = fieldSize . layerField

flushLayer :: Layer -> IO ()
flushLayer = flushWindow . layerField

addLayer :: Field -> IO Layer
addLayer f = do
	ls <- readIORef $ fLayers f
	writeIORef (fLayers f) (ls ++ [[]])
	modifyIORef (fBuffed f) (++ [return ()])
	return Layer{layerField = f, layerId = length ls}

addCharacter :: Field -> IO Character
addCharacter f = do
	cs <- readIORef $ fCharacters f
	writeIORef (fCharacters f) (cs ++ [return ()])
	return Character{characterField = f, characterId = length cs}

runIfOpened :: Field -> IO a -> IO ()
runIfOpened f act = do
	cl <- readIORef $ fClosed f
	if cl then return () else act >> return ()

drawLine :: Layer -> Pixel -> Double -> Double -> Double -> Double -> IO ()
drawLine l@Layer{layerField = f} clr x1 y1 x2 y2 = runIfOpened f $ do
	drawLineBuf f clr fBG x1 y1 x2 y2 >> redrawCharacters f
	addLayerAction l $ whether
		(drawLineBuf f clr fUndoBuf x1 y1 x2 y2)
		(drawLineBuf f clr fBG x1 y1 x2 y2)

clearCharacter :: Character -> IO ()
clearCharacter c = runIfOpened (characterField c) $
	setCharacter c $ return ()

drawCharacter :: Character -> Pixel -> [(Double, Double)] -> IO ()
drawCharacter c@Character{characterField = f} clr sh = do
	runIfOpened (characterField c) $ setCharacter c $ do
		setForeground (fDisplay f) (fGC f) clr
		fillPolygonBuf (characterField c) sh

drawCharacterAndLine ::	Character -> Pixel -> [(Double, Double)] ->
	Double -> Double -> Double -> Double -> IO ()
drawCharacterAndLine c@Character{characterField = f} clr ps x1 y1 x2 y2 = do
	runIfOpened f $ setCharacter c $ do
		setForeground (fDisplay f) (fGC f) clr
		fillPolygonBuf f ps >> drawLineBuf f clr fBuf x1 y1 x2 y2

undoLayer :: Layer -> IO Bool
undoLayer Layer{layerField = f, layerId = lid} = do
	ls <- readIORef $ fLayers f
	if null $ ls !! lid then return False else do
		writeIORef (fLayers f) $ modifyAt ls lid init
		redraw f
		return True

clearLayer :: Layer -> IO ()
clearLayer Layer{layerField = f, layerId = lid} = do
	ls <- readIORef $ fLayers f
	writeIORef (fLayers f) $ setAt ls lid []
	buffed <- readIORef $ fBuffed f
	writeIORef (fBuffed f) $ setAt buffed lid $ return ()
	redrawBuf f
	redraw f

forkIOX :: IO () -> IO ThreadId
forkIOX = (initThreads >>) . forkIO

--------------------------------------------------------------------------------

undoN :: Int
undoN = 100

addLayerAction :: Layer -> (Bool -> IO ()) -> IO ()
addLayerAction Layer{layerField = f, layerId = lid} act = do
	ls <- readIORef $ fLayers f
	if length (ls !! lid) > undoN
		then do	head (ls !! lid) True
			buffed <- readIORef $ fBuffed f
			writeIORef (fBuffed f) $ 
				modifyAt buffed lid (>> head (ls !! lid) True)
			writeIORef (fLayers f) $
				modifyAt ls lid $ (++ [act]) . tail
		else writeIORef (fLayers f) $ modifyAt ls lid (++ [act])

setCharacter :: Character -> IO () -> IO ()
setCharacter Character{characterField = f, characterId = cid} act = do
	cs <- readIORef $ fCharacters f
	writeIORef (fCharacters f) $ setAt cs cid act
	redrawCharacters f
	flushWindow f

fillPolygonBuf :: Field -> [(Double, Double)] -> IO ()
fillPolygonBuf f ps_ = do
	ps <- convertPos f ps_
	fillPolygon (fDisplay f) (fBuf f) (fGC f) (map (uncurry Point) ps)
		nonconvex coordModeOrigin

drawLineBuf :: Field -> Pixel -> (Field -> Pixmap) ->
	Double -> Double -> Double -> Double -> IO ()
drawLineBuf f@Field{fDisplay = dpy, fGC = gc} clr bf x1_ y1_ x2_ y2_ = do
	setForeground (fDisplay f) (fGC f) clr
	[(x1, y1), (x2, y2)] <- convertPos f [(x1_, y1_), (x2_, y2_)]
	X.drawLine dpy (bf f) gc x1 y1 x2 y2

convertPos :: Field -> [(Double, Double)] -> IO [(Position, Position)]
convertPos f ps = do
	(width, height) <- fieldSize f
	return $ (round . (+ width / 2) *** round . (+ height / 2) . negate)
		`map` ps

fieldSize :: Field -> IO (Double, Double)
fieldSize w = fmap (fromIntegral *** fromIntegral) $ winSize w

winSize :: Field -> IO (Dimension, Dimension)
winSize f = do
	width <- readIORef $ fWidth f
	height <- readIORef $ fHeight f
	return (width, height)

redrawAll :: Field -> IO ()
redrawAll f = do
	redrawBuf f
	redraw f
	flushWindow f

redrawBuf :: Field -> IO ()
redrawBuf f = do
	winSize f >>=
		uncurry (fillRectangle (fDisplay f) (fUndoBuf f) (fGCBG f) 0 0)
	readIORef (fBuffed f) >>= sequence_

redraw :: Field -> IO ()
redraw = withLock $ \f -> do
	(width, height) <- winSize f
	copyArea (fDisplay f) (fUndoBuf f) (fBG f) (fGC f) 0 0 width height 0 0
	readIORef (fLayers f) >>= mapM_ ($ False) . concat
	copyArea (fDisplay f) (fBG f) (fBuf f) (fGC f) 0 0 width height 0 0
	readIORef (fCharacters f) >>= sequence_

redrawCharacters :: Field -> IO ()
redrawCharacters = withLock $ \f -> do
	(width, height) <- winSize f
	copyArea (fDisplay f) (fBG f) (fBuf f) (fGC f) 0 0 width height 0 0
	readIORef (fCharacters f) >>= sequence_

flushWindow :: Field -> IO ()
flushWindow = withLock $ \f -> do
	(width, height) <- winSize f
	copyArea (fDisplay f) (fBuf f) (fWindow f) (fGC f) 0 0 width height 0 0
	flush $ fDisplay f

withLock :: (Field -> IO a) -> Field -> IO a
withLock act f = do
	readChan $ fWait f
	ret <- act f
	writeChan (fWait f) ()
	return ret
