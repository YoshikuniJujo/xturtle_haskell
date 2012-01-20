module Graphics.X11.SquareState (
	SquareState(SquareState),
	getSquareState,
	getPosition,
	setPos,
	popPos,
	setSize,
	getDirection,
	setDirection,
	selectShape,
	getUndoNum,
	setUndoNum,
	popUndoNum,
	pushUndoNum,
	popRotHist,
	pushRotHist,
	isPenDown,
	penDown,
	penUp,

	createShape
) where

import Data.IORef
import Control.Arrow

getPosition :: SquareState -> IO (Double, Double)
getPosition = fmap head . readIORef . sHistory

setPos :: SquareState -> Double -> Double -> IO ()
setPos s nx ny = do
	pushHistory s nx ny

popPos :: SquareState -> IO (Double, Double)
popPos s = do
	popHistory s

popHistory :: SquareState -> IO (Double, Double)
popHistory s = do
	let rh = sHistory s
	p : ps <- readIORef rh
	writeIORef rh ps
	return p

pushHistory :: SquareState -> Double -> Double -> IO ()
pushHistory s = curry $ modifyIORef (sHistory s) . (:)

getSize :: SquareState -> IO Double
getSize = readIORef . sSize

setSize :: SquareState -> Double -> IO ()
setSize = writeIORef . sSize

getDirection :: SquareState -> IO Double
getDirection = readIORef . sDir

setDirection :: SquareState -> Double -> IO ()
setDirection = writeIORef . sDir

getShape :: SquareState -> IO [(Double, Double)]
getShape = readIORef . sShape

setShape :: SquareState -> [(Double, Double)] -> IO ()
setShape = writeIORef . sShape

getUndoNum :: SquareState -> IO Int
getUndoNum = readIORef . sUndoN

setUndoNum :: SquareState -> Int -> IO ()
setUndoNum = writeIORef . sUndoN

popUndoNum :: SquareState -> IO Int
popUndoNum s = do
	let	runs = sUndoNs s
	una <- readIORef runs
	case una of
		un : uns -> do
			writeIORef runs uns
			return un
		_ -> return 1

pushUndoNum :: SquareState -> Int -> IO ()
pushUndoNum s = modifyIORef (sUndoNs s) . (:)

popRotHist :: SquareState -> IO (Maybe Double)
popRotHist s = do
	let	rrh = sRotHist s
	r : rs <- readIORef rrh
	writeIORef rrh rs
	return r

pushRotHist :: SquareState -> Maybe Double -> IO ()
pushRotHist s = modifyIORef (sRotHist s) . (:)

isPenDown :: SquareState -> IO Bool
isPenDown = readIORef . sPenDown

penDown :: SquareState -> IO ()
penDown = flip writeIORef True . sPenDown

penUp :: SquareState -> IO ()
penUp = flip writeIORef False . sPenDown

data SquareState = SquareState {
	sHistory :: IORef [(Double, Double)],
	sSize :: IORef Double,
	sDir :: IORef Double,
	sRotHist :: IORef [Maybe Double],
	sShape :: IORef [(Double, Double)],
	sUndoN :: IORef Int,
	sUndoNs :: IORef [Int],
	sPenDown :: IORef Bool
 }

getSquareState :: IO SquareState
getSquareState = do
	h <- newIORef [(0, 0)]
	sr <- newIORef 1
	dr <- newIORef 0
	rsh <- newIORef classic
	run <- newIORef 1
	runs <- newIORef []
	srh <- newIORef []
	rpd <- newIORef True
	return SquareState {
		sHistory = h,
		sSize = sr,
		sShape = rsh,
		sDir = dr,
		sUndoN = run,
		sUndoNs = runs,
		sRotHist = srh,
		sPenDown = rpd
	 }

mkShape ::
	[(Double, Double)] -> Double -> Double -> Double -> Double -> [(Double, Double)]
mkShape sh s d x y =
	map (uncurry (addDoubles (x, y)) . rotatePointD d . mulPoint s) sh

addDoubles :: (Double, Double) -> Double -> Double -> (Double, Double)
addDoubles (x, y) dx dy = (x + dx, y + dy)

rotatePointD :: Double -> (Double, Double) -> (Double, Double)
rotatePointD = rotatePointR . (* pi) . (/ 180)

rotatePointR :: Double -> (Double, Double) -> (Double, Double)
rotatePointR rad (x, y) =
	(x * cos rad - y * sin rad, x * sin rad + y * cos rad)

mulPoint :: Double -> (Double, Double) -> (Double, Double)
mulPoint s (x, y) = (x * s, y * s)

classic :: [(Double, Double)]
classic = clssc ++ reverse (map (second negate) clssc)
	where
	clssc = [
		(- 10, 0),
		(- 16, 6),
		(0, 0)
	 ]

turtle :: [(Double, Double)]
turtle = ttl ++ reverse (map (second negate) ttl)
	where
	ttl = [
		(- 10, 0),
		(- 8, - 3),
		(- 10, - 5),
		(- 7, - 9),
		(- 5, - 6),
		(0, - 8),
		(4, - 7),
		(6, - 10),
		(8, - 7),
		(7, - 5),
		(10, - 2),
		(13, - 3),
		(16, 0)
	 ]

selectShape :: SquareState -> String -> IO ()
selectShape s name = case name of
	"turtle" -> setShape s turtle
	"clasic" -> setShape s classic
	_ -> setShape s classic

createShape :: SquareState -> Double -> Double -> IO [(Double, Double)]
createShape s x y = do
	size <- getSize s
	d <- getDirection s
	sh <- getShape s
	return $ mkShape sh size d x y
