module Graphics.X11.Turtle.Layers(
	Layers,
	Layer,
	Character,
	
	newLayers,
	addLayer,
	addCharacter,

	addLayerAction,
	undoLayer,
	clearLayer,
	setCharacter,

	redrawLayers
) where

import System.IO.Unsafe
import Data.IORef
import Data.List.Tools
import Control.Concurrent

lockChan :: Chan ()
lockChan = unsafePerformIO $ do
	c <- newChan
	writeChan c ()
	return c

withLock2 :: IO a -> IO a
withLock2 act = do
	readChan lockChan
	ret <- act
	writeChan lockChan ()
	return ret

withLock :: Layers -> (Layers -> IO a) -> IO a
withLock ls act = do
	readChan $ lock ls
	ret <- act ls
	writeChan (lock ls) ()
	return ret

data Layers = Layers{
	undoNum :: Int,
	undoLayersAction :: IO (),
	clearLayersAction :: IO (),
	clearCharactersAction :: IO (),
--	flush :: IO (),
	buffed :: [IO ()],
	layers :: [[(IO (), IO ())]],
	characters :: [IO ()],
	lock :: Chan ()
 }

redrawLayers :: IORef Layers -> IO ()
redrawLayers rls = do
	ls <- readIORef rls
	clearLayersAction ls
	sequence_ $ buffed ls
	redrawFromUndo ls

redrawFromUndo :: Layers -> IO ()
redrawFromUndo ls = do
	undoLayersAction ls
	mapM_ snd $ concat $ layers ls
	clearCharactersAction ls
	sequence_ $ characters ls

newLayers :: Int -> IO () -> IO () -> IO () -> IO (IORef Layers)
newLayers un cla ula cca = do
	ls <- newLayers_ un ula cla cca
	newIORef ls

newLayers_ :: Int -> IO () -> IO () -> IO () -> IO Layers
newLayers_ un ula cla cca = do
	l <- newChan
	writeChan l ()
	return Layers{
		undoNum = un,
		undoLayersAction = ula,
		clearLayersAction = cla,
		clearCharactersAction = cca,
--		flush = flsh,
		buffed = [],
		layers = [],
		characters = [],
		lock = l
	 }

data Layer = Layer{
	layerId :: Int,
	layerLayers :: IORef Layers
 }

data Character = Character{
	characterId :: Int,
	characterLayers :: IORef Layers
 }

addLayer :: IORef Layers -> IO Layer
addLayer rls = withLock2 $ do
	ls <- readIORef rls
	let	(lid, nls) = addLayer_ ls
	writeIORef rls nls
	return Layer{layerId = lid, layerLayers = rls}

addLayer_ :: Layers -> (Int, Layers)
addLayer_ ls =
	(length $ layers ls,
		ls{layers = layers ls ++ [[]], buffed = buffed ls ++ [return ()]})

addLayerAction :: Layer -> (IO (), IO ()) -> IO ()
addLayerAction Layer{layerId = lid, layerLayers = rls} acts = withLock2 $
	readIORef rls >>= flip withLock (\ls -> do
		nls <- addLayerAction_ ls lid acts
		writeIORef rls nls)

addLayerAction_ :: Layers -> Int -> (IO (), IO ()) -> IO Layers
addLayerAction_ ls l acts@(_, act) = do
	let actNum = length $ layers ls !! l
	act
	clearCharactersAction ls
	sequence_ $ characters ls
	if actNum < undoNum ls then
			return ls{layers =
				modifyAt (layers ls) l (++ [acts])}
		else do	fst $ head $ layers ls !! l
			return ls{
				layers = modifyAt (layers ls) l
					((++ [acts]) . tail),
				buffed = modifyAt (buffed ls) l
					(>> fst (head $ layers ls !! l))}

undoLayer :: Layer -> IO Bool
undoLayer Layer{layerId = lid, layerLayers = rls} = withLock2 $
	readIORef rls >>= flip withLock (\ls -> do
		mnls <- undoLayer_ ls lid
		maybe (return False)
			((>> return True) . writeIORef rls) mnls)

undoLayer_ :: Layers -> Int -> IO (Maybe Layers)
undoLayer_ ls l =
	if null $ layers ls !! l then return Nothing else do
		let	nls = modifyAt (layers ls) l init
			nlss = ls{layers = nls}
		redrawFromUndo nlss
		return $ Just nlss

clearLayer :: Layer -> IO ()
clearLayer Layer{layerId = lid, layerLayers = rls} = withLock2 $ do
	ls <- readIORef rls
	nls <- clearLayer_ ls lid
	writeIORef rls nls

clearLayer_ :: Layers -> Int -> IO Layers
clearLayer_ ls l = do
	let	nls = setAt (layers ls) l []
		nbf = setAt (buffed ls) l $ return ()
		nlss = ls{layers = nls, buffed = nbf}
	clearLayersAction ls
	sequence_ nbf
	redrawFromUndo nlss
	return nlss

addCharacter :: IORef Layers -> IO Character
addCharacter rls = withLock2 $ do
	ls <- readIORef rls
	let (cid, nls) = addCharacter_ ls
	writeIORef rls nls
	return Character{characterId = cid, characterLayers = rls}

addCharacter_ :: Layers -> (Int, Layers)
addCharacter_ ls =
	(length $ characters ls,
		ls{characters = characters ls ++ [return ()]})

setCharacter :: Character -> IO () -> IO ()
setCharacter Character{characterId = cid, characterLayers = rls} act = withLock2 $
	readIORef rls >>= flip withLock (\ls -> do
	nls <- setCharacter_ ls cid act
	writeIORef rls nls
--	flush ls)
	)

setCharacter_ :: Layers -> Int -> IO () -> IO Layers
setCharacter_ ls c act = do
	let cs = setAt (characters ls) c act
	clearCharactersAction ls
	sequence_ cs
	return ls{characters = cs}
