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
	Display, Pixmap, Point(..), Pixel,

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
import Graphics.X11.Turtle.Layers(undoLayer, clearLayer,
	Layer, Character, newLayers, setCharacter, addLayerAction)
import Graphics.X11.Turtle.FieldType

import Data.Bits((.|.), shift)
import Data.Convertible(convert)
import Data.Maybe

import Control.Monad(replicateM, forM_, forever, replicateM_, when)
import Control.Monad.Tools(doWhile_, whenM, unlessM)
import Control.Concurrent(
	forkIO, ThreadId, Chan, newChan, writeChan, readChan, threadWaitRead,
	killThread)

import System.Posix.Types
import System.Locale.SetLocale

import Text.XML.YJSVG(Color(..))

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
	bufs@[undoBuf, bg, buf] <-
		replicateM 3 $ createPixmap dpy root rWidth rHeight depth
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
							uncurry (fun $ fromIntegral
								$ ev_button ev)
								pos
						| et == buttonRelease -> do
							writeIORef (fPress f) False
							fun <- readIORef
								(fOnrelease f)
							uncurry (fun $ fromIntegral 
								$ ev_button ev)
								pos
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

forkIOX :: IO () -> IO ThreadId
forkIOX = (initThreads >>) . forkIO

flushWindow :: Field -> IO ()
flushWindow = withLock $ \f -> do
	(width, height) <- winSize f
	copyArea (fDisplay f) (fBuf f) (fWindow f) (fGC f) 0 0 width height 0 0
	flush $ fDisplay f

nextNotFilteredEvent :: Display -> XEventPtr -> IO ()
nextNotFilteredEvent dpy e = do
	nextEvent dpy e
	whenM (filterEvent e 0) $ nextNotFilteredEvent dpy e

--------------------------------------------------------------------------------

drawLine :: Field ->
	Layer -> Double -> Color -> Double -> Double -> Double -> Double -> IO ()
drawLine f l lw clr x1 y1 x2 y2 =
	addLayerAction l (drawLineBuf f (round lw) clr fUndoBuf x1 y1 x2 y2,
		drawLineBuf f (round lw) clr fBG x1 y1 x2 y2)

writeString :: Field -> Layer -> String -> Double -> Color ->
	Double -> Double -> String -> IO ()
writeString f l fname size clr x y str =
	addLayerAction l (writeStringBuf f fUndoBuf fname size clr x y str,
		writeStringBuf f fBG fname size clr x y str)

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
drawLineBuf f@Field{fDisplay = dpy, fGC = gc} lw c bf x1_ y1_ x2_ y2_ = do
	clr <- getColorPixel dpy c
	setForeground (fDisplay f) (fGC f) clr
	setLineAttributes (fDisplay f) (fGC f) (fromIntegral lw) lineSolid capRound joinRound
	[(x1, y1), (x2, y2)] <- convertPos f [(x1_, y1_), (x2_, y2_)]
	X.drawLine dpy (bf f) gc x1 y1 x2 y2

fillPolygonBuf :: Field -> [(Double, Double)] -> IO ()
fillPolygonBuf f ps_ = do
	ps <- convertPos f ps_
	fillPolygon (fDisplay f) (fBuf f) (fGC f) (map (uncurry Point) ps)
		nonconvex coordModeOrigin

fieldColor :: Field -> Color -> IO ()
fieldColor f@Field{fDisplay = dpy} c = do
	clr <- getColorPixel dpy c
	setForeground (fDisplay f) (fGCBG f) clr
	let bufs = [fUndoBuf f, fBG f, fBuf f]
	width <- readIORef $ fWidth f
	height <- readIORef $ fHeight f
	forM_ bufs $ \bf -> fillRectangle (fDisplay f) bf (fGCBG f) 0 0 width height

getColorPixel :: Display -> Color -> IO Pixel
getColorPixel _ (RGB r g b) = return $ shift (fromIntegral r) 16 .|.
	shift (fromIntegral g) 8 .|. fromIntegral b
getColorPixel dpy (ColorName cn) = do
	let	scr = defaultScreen dpy
		colormap = defaultColormap dpy scr
	fmap (X.color_pixel . fst) $ allocNamedColor dpy colormap cn

writeStringBuf :: Field -> (Field -> Pixmap) -> String -> Double ->
	Color -> Double -> Double -> String -> IO ()
writeStringBuf f buf fname size clr x_ y_ str = do
	let	dpy = fDisplay f
		scr = defaultScreen dpy
		scrN = defaultScreenOfDisplay dpy
		visual = defaultVisual dpy scr
		colormap = defaultColormap dpy scr
	xftDraw <- xftDrawCreate dpy (buf f) visual colormap
	xftFont <- xftFontOpen dpy scrN $ fname ++ "-" ++ show (round size :: Int)
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
