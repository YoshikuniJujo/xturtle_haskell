module Graphics.X11.Turtle.XTools(
	-- * types
	Display,
	Window,
	Pixmap,
	XIC,
	Atom,
	Point(..),
	Position,
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
	fillRectangle,
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

import Text.XML.YJSVG(Color(..))

import Graphics.X11(
	Display, Drawable, Window, Pixmap, GC, Pixel, Atom, Point(..), Position,
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

data Bufs = Bufs{undoBuf :: Pixmap, bgBuf :: Pixmap, topBuf :: Pixmap}
data GCs = GCs{gcForeground :: GC, gcBackground :: GC}

--------------------------------------------------------------------------------

forkIOX :: IO () -> IO ThreadId
forkIOX = (initThreads >>) . forkIO

openWindow :: IO (Display, Window, Bufs, GCs, XIC, Atom, (Dimension, Dimension))
openWindow = do
	_ <- setLocale LC_CTYPE Nothing >>= maybe (error "setLocale") return
	_ <- initThreads
	unlessM supportsLocale $ error "Current locale is not supported."
	_ <- setLocaleModifiers ""
	dpy <- openDisplay ""
	del <- internAtom dpy "WM_DELETE_WINDOW" True
	let	scr = defaultScreen dpy
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

colorPixel :: Display -> Color -> IO (Maybe Pixel)
colorPixel _ (RGB r g b) = return $ Just $ shift (fromIntegral r) 16 .|.
	shift (fromIntegral g) 8 .|. fromIntegral b
colorPixel dpy (ColorName cn) = fmap (Just . color_pixel . fst)
	(allocNamedColor dpy (defaultColormap dpy $ defaultScreen dpy) cn)
		`catch` const (putStrLn "no such color" >> return Nothing)

setForegroundXT :: Display -> GC -> Color -> IO ()
setForegroundXT dpy gc clr =
	colorPixel dpy clr >>= maybe (return()) (setForeground dpy gc)

fillPolygonXT :: Display -> Drawable -> GC -> [Point] -> IO ()
fillPolygonXT d w gc ps = fillPolygon d w gc ps nonconvex coordModeOrigin

drawLineXT :: Display -> GC -> Drawable -> Int -> Color ->
	Position -> Position -> Position -> Position -> IO ()
drawLineXT dpy gc buf lw c x1 y1 x2 y2 = do
	colorPixel dpy c >>= maybe (return ()) (setForeground dpy gc)
	setLineAttributes dpy gc (fromIntegral lw) lineSolid capRound joinRound
	drawLine dpy buf gc x1 y1 x2 y2

writeStringXT :: Display -> Drawable -> String -> Double -> Color ->
	Position -> Position -> String -> IO ()
writeStringXT dpy buf fname size clr x y str = do
	let	visual = defaultVisual dpy $ defaultScreen dpy
		colormap = defaultColormap dpy $ defaultScreen dpy
		font = fname ++ "-" ++ showFFloat (Just 0) size ""
	xftDraw <- xftDrawCreate dpy buf visual colormap
	xftFont <- xftFontOpen dpy (defaultScreenOfDisplay dpy) font
	case clr of
		RGB r g b -> withXftColorValue dpy visual colormap color $ \c ->
			xftDrawString xftDraw c xftFont x y str
			where
			color = XRenderColor {
				xrendercolor_red = fromIntegral r * 0x100,
				xrendercolor_green = fromIntegral b * 0x100,
				xrendercolor_blue = fromIntegral g * 0x100,
				xrendercolor_alpha = 0xffff}
		ColorName cn -> withXftColorName dpy visual colormap cn $ \c ->
			xftDrawString xftDraw c xftFont x y str
			
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
			let	zero = 0 :: Int
			w <- fmap fromIntegral imageGetWidth :: IO Int
			h <- fmap fromIntegral imageGetHeight :: IO Int
			img' <- createCroppedScaledImage zero zero w h nw nh
			contextSetImage img'
			fmap Just imageGetData
		_ -> print err >> return Nothing

drawBitmap :: Display -> Drawable -> GC -> Position -> Position -> Dimension ->
	Dimension -> Ptr Word32 -> IO ()
drawBitmap dpy win gc x0 y0 w h dat = do
	ptr <- newIORef dat
	forM_ [0 .. w * h - 1] $ \i -> do
		readIORef ptr >>= peek >>= setForeground dpy gc
		let	x = fromIntegral i `mod` w
			y = fromIntegral i `div` w
		drawPoint dpy win gc (x0 + fromIntegral x) (y0 + fromIntegral y)
		atomicModifyIORef_ ptr $ flip advancePtr 1

--------------------------------------------------------------------------------

waitEvent :: Display -> IO ()
waitEvent = threadWaitRead . Fd . connectionNumber
