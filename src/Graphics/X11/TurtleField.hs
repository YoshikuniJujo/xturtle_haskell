module Graphics.X11.TurtleField(
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
	drawLineNotFlush,
	writeString,
	drawCharacter,
	drawCharacterAndLine,
	clearCharacter,

	undoLayer,
	clearLayer,
	flushLayer,

	onclick,

	forkIOX,
	addThread,

	Color(..)
) where

import Graphics.X11(
	Display, Window, Pixmap, Atom, GC, Point(..), Dimension, Position, Pixel,

	setLineAttributes, lineSolid, capRound, joinRound,

	openDisplay, closeDisplay, flush, defaultScreen, rootWindow,
	whitePixel, blackPixel,	defaultDepth,
	createSimpleWindow, mapWindow, createPixmap, internAtom, createGC,

	setForeground, copyArea,
	fillRectangle, fillPolygon, nonconvex, coordModeOrigin,

	setWMProtocols, selectInput, allocaXEvent, nextEvent,
	keyPressMask, exposureMask, buttonPressMask,

	getGeometry, initThreads, connectionNumber, pending, destroyWindow,

	defaultVisual, defaultColormap, defaultScreenOfDisplay
 )
import qualified Graphics.X11 as X (drawLine)
import Graphics.X11.Xlib.Extras(Event(..), getEvent)
import Graphics.X11.Xft
import Graphics.X11.Xrender

import Data.IORef(IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Bits((.|.))
import Data.Convertible(convert)
import Data.List.Tools(modifyAt, setAt)
import Data.Bool.Tools(whether)

import Control.Monad(replicateM, forM_, forever, replicateM_, when, unless)
import Control.Monad.Tools(doWhile_)
import Control.Arrow((***))
import Control.Concurrent(
	forkIO, ThreadId, Chan, newChan, writeChan, readChan, threadWaitRead,
	killThread)

import System.Posix.Types
import Foreign.C.Types

data Color = Color {pixel :: Pixel}

colorToPixel :: Color -> Pixel
colorToPixel = pixel

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
	fRunning :: IORef [ThreadId],
	fOnclick :: IORef (Double -> Double -> IO Bool),
	fEnd :: Chan ()
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
	selectInput dpy win $ exposureMask .|. keyPressMask .|. buttonPressMask
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
	onclickRef <- newIORef $ const $ const $ return True
	endRef <- newChan
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
		fRunning = running,
		fOnclick = onclickRef,
		fEnd = endRef
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
	(>> (closeDisplay (fDisplay f) >> writeChan (fEnd f) ())) $
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
			Just ev@ButtonEvent{} -> do
				pos <- convertPosRev f (ev_x ev) (ev_y ev)
				readIORef (fOnclick f) >>= ($ pos) . uncurry
			Just ev@ClientMessageEvent{} ->
				return $ convert (head $ ev_data ev) /= fDel f
			Nothing -> killThread th1 >> return False
			_ -> return True

onclick :: Field -> (Double -> Double -> IO Bool) -> IO ()
onclick f = writeIORef (fOnclick f)

fieldColor :: Field -> Color -> IO ()
fieldColor f clr = do
	setForeground (fDisplay f) (fGCBG f) $ colorToPixel clr
	let bufs = [fUndoBuf f, fBG f, fBuf f]
	width <- readIORef $ fWidth f
	height <- readIORef $ fHeight f
	forM_ bufs $ \bf -> fillRectangle (fDisplay f) bf (fGCBG f) 0 0 width height
	redrawAll f

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
	writeChan (fClose f) ()
	writeIORef (fClosed f) True

addThread :: Field -> ThreadId -> IO ()
addThread f tid = modifyIORef (fRunning f) (tid :)

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
	unless cl $ act >> return ()

drawLineNotFlush ::
	Layer -> Double -> Color -> Double -> Double -> Double -> Double -> IO ()
drawLineNotFlush l@Layer{layerField = f} lw_ clr x1 y1 x2 y2 = runIfOpened f $ do
	drawLineBuf f lw clr fBG x1 y1 x2 y2
	addLayerAction l $ whether
		(drawLineBuf f lw clr fUndoBuf x1 y1 x2 y2)
		(drawLineBuf f lw clr fBG x1 y1 x2 y2)
	where
	lw = round lw_

drawLine :: Layer -> Double -> Color -> Double -> Double -> Double -> Double -> IO ()
drawLine l@Layer{layerField = f} lw_ clr x1 y1 x2 y2 = runIfOpened f $ do
	drawLineBuf f lw clr fBG x1 y1 x2 y2 >> redrawCharacters f
	addLayerAction l $ whether
		(drawLineBuf f lw clr fUndoBuf x1 y1 x2 y2)
		(drawLineBuf f lw clr fBG x1 y1 x2 y2)
	where
	lw = round lw_

writeString :: Layer -> String -> Double -> Double -> Double -> Double ->
	Double -> Double -> String -> IO ()
writeString l@Layer{layerField = f} fname size r g b x y str = do
	writeStringBuf f fBG fname size r g b x y str
	redrawCharacters f
	addLayerAction l $ whether
		(writeStringBuf f fUndoBuf fname size r g b x y str)
		(writeStringBuf f fBG fname size r g b x y str)

writeStringBuf :: Field -> (Field -> Pixmap) -> String -> Double -> Double -> Double -> Double ->
	Double -> Double -> String -> IO ()
writeStringBuf f buf fname size r g b x_ y_ str = do
	let	dpy = fDisplay f
		scr = defaultScreen dpy
		scrN = defaultScreenOfDisplay dpy
		visual = defaultVisual dpy scr
		colormap = defaultColormap dpy scr
	xftDraw <- xftDrawCreate dpy (buf f) visual colormap
	xftFont <- xftFontOpen dpy scrN $ fname ++ "-" ++ show (round size :: Int) -- "KochiGothic-20"
	[(x, y)] <- convertPos f [(x_, y_)]
	withXftColorValue dpy visual colormap color $ \c ->
		xftDrawString xftDraw c xftFont x y str
	where
	color = XRenderColor {
		xrendercolor_red = round $ r * 0xffff,
		xrendercolor_green = round $ b * 0xffff,
		xrendercolor_blue = round $ g * 0xffff,
		xrendercolor_alpha = 0xffff
	 }

clearCharacter :: Character -> IO ()
clearCharacter c = runIfOpened (characterField c) $
	setCharacter c $ return ()

drawCharacter :: Character -> Color -> [(Double, Double)] -> IO ()
drawCharacter c@Character{characterField = f} clr sh =
	runIfOpened (characterField c) $ setCharacter c $ do
		setForeground (fDisplay f) (fGC f) $ colorToPixel clr
		fillPolygonBuf (characterField c) sh

drawCharacterAndLine ::	Character -> Color -> [(Double, Double)] -> Double ->
	Double -> Double -> Double -> Double -> IO ()
drawCharacterAndLine c@Character{characterField = f} clr ps lw_ x1 y1 x2 y2 =
	runIfOpened f $ setCharacter c $ do
		setForeground (fDisplay f) (fGC f) $ colorToPixel clr
		fillPolygonBuf f ps >> drawLineBuf f lw clr fBuf x1 y1 x2 y2
	where
	lw = round lw_

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

drawLineBuf :: Field -> Int -> Color -> (Field -> Pixmap) ->
	Double -> Double -> Double -> Double -> IO ()
drawLineBuf f@Field{fDisplay = dpy, fGC = gc} lw clr bf x1_ y1_ x2_ y2_ = do
	setForeground (fDisplay f) (fGC f) $ colorToPixel clr
	setLineAttributes (fDisplay f) (fGC f) (fromIntegral lw) lineSolid capRound joinRound
	[(x1, y1), (x2, y2)] <- convertPos f [(x1_, y1_), (x2_, y2_)]
	X.drawLine dpy (bf f) gc x1 y1 x2 y2

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

waitField :: Field -> IO ()
waitField = readChan . fEnd
