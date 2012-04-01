module Graphics.X11.Turtle.XTools(
	-- * types
	Display,
	Window,
	Pixmap,
	XIC,
	Atom,
	Point(..),
	PositionXT,
	Dimension,
	Bufs,
	undoBuf,
	bgBuf,
	topBuf,
	GCs,
	gcForeground,
	gcBackground,

	-- ** event types
	XEventPtr,
	Event(..),

	-- * basic functions
	forkIOX,
	openWindow,
	destroyWindow,
	closeDisplay,
	windowSize,

	-- * draw functions
	flush,
	setForegroundXT,
	copyArea,
	fillRectangleXT,
	fillPolygonXT,
	drawLineXT,
	writeStringXT,
	drawImageXT,

	-- * event functions
	allocaXEvent,
	waitEvent,
	pending,
	nextEvent,
	getEvent,
	filterEvent,
	utf8LookupString,
	buttonPress,
	buttonRelease,
	xK_VoidSymbol,
) where

import Graphics.X11(
	Display, Drawable, Window, Pixmap, GC, Atom, Point(..), Position,
	Dimension, XEventPtr,
	initThreads, flush, supportsLocale, setLocaleModifiers,
	connectionNumber, openDisplay, closeDisplay, internAtom,
	createSimpleWindow, destroyWindow, mapWindow, getGeometry,
	createGC, createPixmap, rootWindow, defaultScreen,
	defaultScreenOfDisplay, defaultVisual, defaultColormap, defaultDepth,
	whitePixel, blackPixel,
	copyArea, fillRectangle, fillPolygon, drawLine, drawPoint,
	nonconvex, coordModeOrigin,
	setLineAttributes, lineSolid, capRound, joinRound, setForeground,
	allocNamedColor, color_pixel,
	setWMProtocols, selectInput, allocaXEvent, pending, nextEvent,
	exposureMask, keyPressMask, buttonPressMask, buttonReleaseMask,
	pointerMotionMask, buttonPress, buttonRelease, xK_VoidSymbol)
import Graphics.X11.Xlib.Extras(Event(..), getEvent)
import Graphics.X11.Xft(
	xftDrawCreate, xftFontOpen, withXftColorValue, withXftColorName,
	xftDrawString)
import Graphics.X11.Xrender(XRenderColor(..))
import Graphics.X11.Xim(
	XIC, XNInputStyle(..), openIM, createIC, getICValue, filterEvent,
	utf8LookupString)
import Graphics.Imlib(
	ImlibLoadError(..), loadImageWithErrorReturn, contextSetImage,
	imageGetWidth, imageGetHeight, imageGetData, createCroppedScaledImage)
import Text.XML.YJSVG(Color(..))
import Numeric(showFFloat)
import Control.Monad(forM_, replicateM)
import Control.Monad.Tools(unlessM)
import Control.Concurrent(ThreadId, forkIO, threadWaitRead)
import System.Locale.SetLocale(Category(..), setLocale)
import System.Posix.Types(Fd(..))
import Data.Word(Word32)
import Data.Bits((.|.), shift)
import Data.IORef(newIORef, readIORef)
import Data.IORef.Tools(atomicModifyIORef_)
import Foreign.Ptr(Ptr)
import Foreign.Storable(peek)
import Foreign.Marshal.Array(advancePtr)

--------------------------------------------------------------------------------

type PositionXT = Position
data Bufs = Bufs{undoBuf :: Pixmap, bgBuf :: Pixmap, topBuf :: Pixmap}
data GCs = GCs{gcForeground :: GC, gcBackground :: GC}

--------------------------------------------------------------------------------

forkIOX :: IO () -> IO ThreadId
forkIOX = forkIO . (initThreads >>)

openWindow :: IO (Display, Window, Bufs, GCs, XIC, Atom, (Dimension, Dimension))
openWindow = do
	_ <- setLocale LC_CTYPE Nothing >>= maybe (error "setLocale") return
	_ <- initThreads
	unlessM supportsLocale $ error "Current locale is not supported."
	_ <- setLocaleModifiers ""
	dpy <- openDisplay ""
	del <- internAtom dpy "WM_DELETE_WINDOW" True
	let scr = defaultScreen dpy
	root <- rootWindow dpy scr
	(rWidth, rHeight) <- windowSize dpy root
	bufs@[ub, bb, tb] <- replicateM 3 $
		createPixmap dpy root rWidth rHeight $ defaultDepth dpy scr
	win <- createSimpleWindow dpy root 0 0 rWidth rHeight 1
		(blackPixel dpy scr) (whitePixel dpy scr)
	im <- openIM dpy Nothing Nothing Nothing
	ic <- createIC im [XIMPreeditNothing, XIMStatusNothing] win
	fevent <- getICValue ic "filterEvents"
	[gc, gcBG] <- replicateM 2 $ createGC dpy win
	setForeground dpy gcBG 0xffffff
	forM_ bufs $ \buf -> fillRectangle dpy buf gcBG 0 0 rWidth rHeight
	setWMProtocols dpy win [del]
	selectInput dpy win $ fevent .|. exposureMask .|. keyPressMask .|.
		buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask
	size <- mapWindow dpy win >> windowSize dpy win
	return (dpy, win, Bufs ub bb tb, GCs gc gcBG, ic, del, size)

windowSize :: Display -> Window -> IO (Dimension, Dimension)
windowSize dpy win = do
	(_, _, _, width, height, _, _) <- getGeometry dpy win
	return (width, height)

--------------------------------------------------------------------------------

setForegroundXT :: Display -> GC -> Color -> IO ()
setForegroundXT dpy gc (RGB r_ g_ b_) = let
	[r, g, b] = map fromIntegral [r_, g_, b_] in
	setForeground dpy gc $ shift r 16 .|. shift g 8 .|. b
setForegroundXT dpy gc (ColorName cn) = let
	cm = defaultColormap dpy $ defaultScreen dpy in
	(allocNamedColor dpy cm cn >>= setForeground dpy gc . color_pixel . fst)
		`catch` const (putStrLn "no such color")

fillRectangleXT :: Display -> Drawable -> GC -> Position -> Position ->
	Dimension -> Dimension -> IO ()
fillRectangleXT = fillRectangle

fillPolygonXT :: Display -> Drawable -> GC -> [Point] -> IO ()
fillPolygonXT d w gc ps = fillPolygon d w gc ps nonconvex coordModeOrigin

drawLineXT :: Display -> GC -> Drawable -> Int -> Color ->
	Position -> Position -> Position -> Position -> IO ()
drawLineXT dpy gc buf lw clr x1 y1 x2 y2 = do
	setForegroundXT dpy gc clr
	setLineAttributes dpy gc (fromIntegral lw) lineSolid capRound joinRound
	drawLine dpy buf gc x1 y1 x2 y2

writeStringXT :: Display -> Drawable -> String -> Double -> Color ->
	Position -> Position -> String -> IO ()
writeStringXT dpy buf fname size clr x y str = do
	let	vsl = defaultVisual dpy $ defaultScreen dpy
		cm = defaultColormap dpy $ defaultScreen dpy
		withColor = case clr of
			RGB r g b -> withXftColorValue dpy vsl cm XRenderColor{
				xrendercolor_red = fromIntegral r * 0x100,
				xrendercolor_blue = fromIntegral g * 0x100,
				xrendercolor_green = fromIntegral b * 0x100,
				xrendercolor_alpha = 0xffff}
			ColorName cn -> withXftColorName dpy vsl cm cn
	draw <- xftDrawCreate dpy buf vsl cm
	font <- xftFontOpen dpy (defaultScreenOfDisplay dpy) $
		fname ++ "-" ++ showFFloat (Just 0) size ""
	withColor $ \c -> xftDrawString draw c font x y str
			
drawImageXT :: Display -> Drawable -> GC -> FilePath -> Position -> Position ->
	Dimension -> Dimension -> IO ()
drawImageXT dpy win gc fp x y w h =
	getImage fp w h >>= maybe (return ()) (drawBitmap dpy win gc x y w h)

getImage :: FilePath -> Dimension -> Dimension -> IO (Maybe (Ptr Word32))
getImage fp nw nh = do
	(img, err) <- loadImageWithErrorReturn fp
	case err of
		ImlibLoadErrorNone -> do
			contextSetImage img
			let	zero = 0 :: Position
			w <- fmap fromIntegral imageGetWidth :: IO Dimension
			h <- fmap fromIntegral imageGetHeight :: IO Dimension
			img' <- createCroppedScaledImage zero zero w h nw nh
			contextSetImage img'
			fmap Just imageGetData
		_ -> print err >> return Nothing

drawBitmap :: Display -> Drawable -> GC -> Position -> Position -> Dimension ->
	Dimension -> Ptr Word32 -> IO ()
drawBitmap dpy win gc x0 y0 w_ h_ dat = do
	ptr <- newIORef dat
	forM_ [0 .. w * h - 1]  $ \i -> do
		readIORef ptr >>= peek >>= setForeground dpy gc
		drawPoint dpy win gc (x0 + i `mod` w) (y0 + i `div` w)
		atomicModifyIORef_ ptr $ flip advancePtr 1
	where [w, h] = map fromIntegral [w_, h_]

--------------------------------------------------------------------------------

waitEvent :: Display -> IO ()
waitEvent = threadWaitRead . Fd . connectionNumber
