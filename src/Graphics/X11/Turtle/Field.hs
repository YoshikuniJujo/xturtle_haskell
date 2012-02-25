module Graphics.X11.Turtle.Field(
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

import Graphics.X11(
	Display, Window, Pixmap, Atom, GC, Point(..), Dimension, Position, Pixel,

	setLineAttributes, lineSolid, capRound, joinRound,

	openDisplay, closeDisplay, flush, defaultScreen, rootWindow,
	whitePixel, blackPixel,	defaultDepth,
	createSimpleWindow, mapWindow, createPixmap, internAtom, createGC,

	setForeground, copyArea,
	fillRectangle, fillPolygon, nonconvex, coordModeOrigin,

	setWMProtocols, selectInput, allocaXEvent, nextEvent, XEventPtr,
	keyPressMask, exposureMask, buttonPressMask, buttonReleaseMask,
	button1MotionMask,

	getGeometry, initThreads, connectionNumber, pending, destroyWindow,

	defaultVisual, defaultColormap, defaultScreenOfDisplay,
	Visual, Colormap,

	supportsLocale, setLocaleModifiers,
	xK_VoidSymbol, buttonPress, buttonRelease,
	allocNamedColor
 )
import qualified Graphics.X11 as X (drawLine, Color(..))
import Graphics.X11.Xlib.Extras(Event(..), getEvent)
import Graphics.X11.Xft
import Graphics.X11.Xrender
import Graphics.X11.Xim
import qualified Graphics.X11.Turtle.Layers as L

import Data.IORef(IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Bits((.|.), shift)
import Data.Convertible(convert)
import Data.List.Tools(modifyAt, setAt)
import Data.Bool.Tools(whether)
import Data.Maybe

import Control.Monad(replicateM, forM_, forever, replicateM_, when, unless)
import Control.Monad.Tools(doWhile_, whenM, unlessM)
import Control.Arrow((***))
import Control.Concurrent(
	forkIO, ThreadId, Chan, newChan, writeChan, readChan, threadWaitRead,
	killThread)

import System.Posix.Types
import System.Locale.SetLocale
import Foreign.C.Types

import Text.XML.YJSVG(Color(..))

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

	fLLayers :: IORef L.Layers,

	fBuffed :: IORef [IO ()],
	fLayers :: IORef [[Bool -> IO ()]],
	fCharacters :: IORef [IO ()],

	fWait :: Chan (),
	fEvent :: Chan (Maybe Event),
	fClose :: Chan (),
	fClosed :: IORef Bool,
	fRunning :: IORef [ThreadId],
	fOnclick :: IORef (Int -> Double -> Double -> IO Bool),
	fOnrelease :: IORef (Double -> Double -> IO Bool),
	fOndrag :: IORef (Double -> Double -> IO ()),
	fPress :: IORef Bool,
	fKeypress :: IORef (Char -> IO Bool),
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
	_ <- setLocale LC_CTYPE Nothing >>= maybe (error "Can't set locale.") return
	_ <- initThreads
	unlessM supportsLocale $ error "Current locale is notSupported."
	_ <- setLocaleModifiers ""
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
	im <- openIM dpy Nothing Nothing Nothing
	ic <- createIC im [XIMPreeditNothing, XIMStatusNothing] win
	fevent <- getICValue ic "filterEvents"
	[gc, gcBG] <- replicateM 2 $ createGC dpy win
	setForeground dpy gcBG 0xffffff
	forM_ bufs $ \bf -> fillRectangle dpy bf gcBG 0 0 rWidth rHeight
	setWMProtocols dpy win [del]
	selectInput dpy win $
		exposureMask .|. keyPressMask .|.
		buttonPressMask .|. buttonReleaseMask .|. button1MotionMask .|.
		fevent
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
	onclickRef <- newIORef $ const $ const $ const $ return True
	onreleaseRef <- newIORef $ const $ const $ return True
	ondragRef <- newIORef $ const $ const $ return ()
	pressRef <- newIORef False
	keypressRef <- newIORef $ const $ return True
	endRef <- newChan
	writeChan wait ()
	fllRef <- newIORef undefined
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
		fOnrelease = onreleaseRef,
		fOndrag = ondragRef,
		fPress = pressRef,
		fKeypress = keypressRef,
		fEnd = endRef,
		fLLayers = fllRef
	 }
	_ <- forkIOX $ runLoop ic f
	flushWindow f
	fll <- L.newLayers 50 
		(winSize f >>= \(width, height) ->
			copyArea (fDisplay f) (fUndoBuf f) (fBG f) (fGC f) 0 0 width height 0 0)
		(winSize f >>=
			uncurry (fillRectangle (fDisplay f) (fUndoBuf f) (fGCBG f) 0 0))
		(winSize f >>= \(width, height) ->
			copyArea (fDisplay f) (fBG f) (fBuf f) (fGC f) 0 0 width height 0 0)
	writeIORef fllRef fll
	return f{fLLayers = fllRef}

runLoop :: XIC -> Field -> IO ()
runLoop ic f = allocaXEvent $ \e -> do
	endc <- waitInput f
	th1 <- forkIOX $ forever $ do
		evN <- pending $ fDisplay f
		replicateM_ (fromIntegral evN) $ do
			nextNotFilteredEvent (fDisplay f) e
			ev <- getEvent e
			writeChan (fEvent f) $ Just ev
		end <- readChan endc
		when end $ writeChan (fEvent f) Nothing
	doWhile_ $ do
		mev <- readChan $ fEvent f
		case mev of
			Just (ExposeEvent{}) -> do
				(_, _, _, width, height, _, _) <-
					getGeometry (fDisplay f) (fWindow f)
				writeIORef (fWidth f) width
				writeIORef (fHeight f) height
				redrawAll f
				return True
			Just (KeyEvent{}) -> do
				(mstr, mks) <- utf8LookupString ic e
				let	str = fromMaybe " " mstr
					_ks = fromMaybe xK_VoidSymbol mks
				readIORef (fKeypress f) >>= fmap and . ($ str) . mapM
			Just ev@ButtonEvent{} -> do
				pos <- convertPosRev f (ev_x ev) (ev_y ev)
				case ev_event_type ev of
					et	| et == buttonPress -> do
							writeIORef (fPress f) True
							fun <- readIORef (fOnclick f)
							uncurry (fun $ fromIntegral $ ev_button ev)
								pos
						| et == buttonRelease -> do
							writeIORef (fPress f) False
							readIORef (fOnrelease f) >>=
								($ pos) . uncurry
					_ -> error "not implement event"
			Just ev@MotionEvent{} -> do
				pos <- convertPosRev f (ev_x ev) (ev_y ev)
				whenM (readIORef $ fPress f) $
					readIORef (fOndrag f) >>= ($ pos) . uncurry
				return True
			Just ev@ClientMessageEvent{} ->
				return $ convert (head $ ev_data ev) /= fDel f
			Nothing -> killThread th1 >> return False
			_ -> return True
	destroyWindow (fDisplay f) (fWindow f)
	closeDisplay $ fDisplay f
	writeChan (fEnd f) ()

onclick :: Field -> (Int -> Double -> Double -> IO Bool) -> IO ()
onrelease :: Field -> (Double -> Double -> IO Bool) -> IO ()
onclick f = writeIORef $ fOnclick f
onrelease f = writeIORef $ fOnrelease f

ondrag :: Field -> (Double -> Double -> IO ()) -> IO ()
ondrag f = writeIORef $ fOndrag f

onkeypress :: Field -> (Char -> IO Bool) -> IO ()
onkeypress f = writeIORef $ fKeypress f

fieldColor :: Field -> Color -> IO ()
fieldColor f@Field{fDisplay = dpy} c = do
	clr <- getColorPixel dpy c
	setForeground (fDisplay f) (fGCBG f) clr
	let bufs = [fUndoBuf f, fBG f, fBuf f]
	width <- readIORef $ fWidth f
	height <- readIORef $ fHeight f
	forM_ bufs $ \bf -> fillRectangle (fDisplay f) bf (fGCBG f) 0 0 width height
	redrawAll f

getColorPixel :: Display -> Color -> IO Pixel
getColorPixel _ (RGB r g b) = return $ shift (fromIntegral r) 16 .|.
	shift (fromIntegral g) 8 .|. fromIntegral b
getColorPixel dpy (ColorName cn) = do
	let	scr = defaultScreen dpy
		colormap = defaultColormap dpy scr
	fmap (X.color_pixel . fst) $ allocNamedColor dpy colormap cn

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

drawLine ::
	Layer -> Double -> Color -> Double -> Double -> Double -> Double -> IO ()
drawLine l@Layer{layerField = f} lw_ clr x1 y1 x2 y2 = runIfOpened f $ do
	drawLineBuf f lw clr fBG x1 y1 x2 y2
	addLayerAction l $ whether
		(drawLineBuf f lw clr fUndoBuf x1 y1 x2 y2)
		(drawLineBuf f lw clr fBG x1 y1 x2 y2)
	where
	lw = round lw_

writeString :: Layer -> String -> Double -> Color ->
	Double -> Double -> String -> IO ()
writeString l@Layer{layerField = f} fname size clr x y str = do
	writeStringBuf f fBG fname size clr x y str
	redrawCharacters f
	addLayerAction l $ whether
		(writeStringBuf f fUndoBuf fname size clr x y str)
		(writeStringBuf f fBG fname size clr x y str)

writeStringBuf :: Field -> (Field -> Pixmap) -> String -> Double ->
	Color -> Double -> Double -> String -> IO ()
writeStringBuf f buf fname size clr x_ y_ str = do
	let	dpy = fDisplay f
		scr = defaultScreen dpy
		scrN = defaultScreenOfDisplay dpy
		visual = defaultVisual dpy scr
		colormap = defaultColormap dpy scr
	xftDraw <- xftDrawCreate dpy (buf f) visual colormap
	xftFont <- xftFontOpen dpy scrN $ fname ++ "-" ++ show (round size :: Int) -- "KochiGothic-20"
	[(x, y)] <- convertPos f [(x_, y_)]
	withXftColor dpy visual colormap clr $ \c ->
		xftDrawString xftDraw c xftFont x y str

withXftColor ::
	Display -> Visual -> Colormap -> Color -> (XftColor -> IO a) -> IO a
withXftColor dpy visual colormap (RGB r g b) action =
	withXftColorValue dpy visual colormap color action
	where
	color = XRenderColor {
		xrendercolor_red = fromIntegral r * 0x100,
		xrendercolor_green = fromIntegral b * 0x100,
		xrendercolor_blue = fromIntegral g * 0x100,
		xrendercolor_alpha = 0xffff
	 }
withXftColor dpy visual colormap (ColorName cn) action =
	withXftColorName dpy visual colormap cn action

clearCharacter :: Character -> IO ()
clearCharacter c = runIfOpened (characterField c) $
	setCharacter c $ return ()

drawCharacter :: Character -> Color -> [(Double, Double)] -> IO ()
drawCharacter c@Character{characterField = f} cl sh =
	runIfOpened (characterField c) $ setCharacter c $ do
		clr <- getColorPixel (fDisplay f) cl
		setForeground (fDisplay f) (fGC f) clr
		fillPolygonBuf (characterField c) sh

drawCharacterAndLine ::	Character -> Color -> [(Double, Double)] -> Double ->
	Double -> Double -> Double -> Double -> IO ()
drawCharacterAndLine c@Character{characterField = f} cl ps lw_ x1 y1 x2 y2 =
	runIfOpened f $ setCharacter c $ do
		clr <- getColorPixel (fDisplay f) cl
		setForeground (fDisplay f) (fGC f) clr
		fillPolygonBuf f ps >> drawLineBuf f lw cl fBuf x1 y1 x2 y2
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
drawLineBuf f@Field{fDisplay = dpy, fGC = gc} lw c bf x1_ y1_ x2_ y2_ = do
	clr <- getColorPixel dpy c
	setForeground (fDisplay f) (fGC f) clr
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

nextNotFilteredEvent :: Display -> XEventPtr -> IO ()
nextNotFilteredEvent dpy e = do
	nextEvent dpy e
	whenM (filterEvent e 0) $ nextNotFilteredEvent dpy e
