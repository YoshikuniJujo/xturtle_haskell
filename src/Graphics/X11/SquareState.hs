module Graphics.X11.SquareState (
	SquareState(SquareState),
	getSquareState,
	getPosition,
	setPosition,
	popHistory,
	pushHistory,
	getSize,
	setSize,
	getDirection,
	setDirection,
	getShape,
	setShape,
	getUndoNum,
	setUndoNum,
	popUndoNum,
	pushUndoNum,
	popRotHist,
	pushRotHist,
	isPenDown,
	penDown,
	penUp

--	sRotHist,
--	sPenDown
) where

import Data.IORef

getPosition :: SquareState -> IO (Double, Double)
getPosition = readIORef . sPos

setPosition :: SquareState -> Double -> Double -> IO ()
setPosition = curry . writeIORef . sPos

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
	sPos :: IORef (Double, Double),
	sHistory :: IORef [(Double, Double)],
	sSize :: IORef Double,
	sDir :: IORef Double,
	sShape :: IORef [(Double, Double)],
	sUndoN :: IORef Int,
	sUndoNs :: IORef [Int],
	sIsRotated :: IORef Bool,
	sRotHist :: IORef [Maybe Double],
	sPenDown :: IORef Bool
 }

getSquareState :: IO SquareState
getSquareState = do
	p <- newIORef undefined
	h <- newIORef []
	sr <- newIORef 1
	dr <- newIORef 0
	rsh <- newIORef []
	run <- newIORef 1
	runs <- newIORef []
	isr <- newIORef False
	srh <- newIORef []
	rpd <- newIORef True
	return SquareState {
		sPos = p,
		sHistory = h,
		sSize = sr,
		sShape = rsh,
		sDir = dr,
		sUndoN = run,
		sUndoNs = runs,
		sIsRotated = isr,
		sRotHist = srh,
		sPenDown = rpd
	 }
