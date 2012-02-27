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
) where

import Data.IORef
import Data.List.Tools

data Layers = Layers{
	undoNum :: Int,
	undoLayersAction :: IO (),
	clearLayersAction :: IO (),
	clearCharactersAction :: IO (),
	buffed :: [IO ()],
	layers :: [[(IO (), IO ())]],
	characters :: [IO ()]
 }

newLayers :: Int -> IO () -> IO () -> IO () -> IO (IORef Layers)
newLayers un ula cla cca = do
	ls <- newLayers_ un ula cla cca
	newIORef ls

newLayers_ :: Int -> IO () -> IO () -> IO () -> IO Layers
newLayers_ un ula cla cca = do
	return Layers{
		undoNum = un,
		undoLayersAction = ula,
		clearLayersAction = cla,
		clearCharactersAction = cca,
		buffed = [],
		layers = [],
		characters = []
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
addLayer rls = do
	ls <- readIORef rls
	let	(lid, nls) = addLayer_ ls
	writeIORef rls nls
	return Layer{layerId = lid, layerLayers = rls}

addLayer_ :: Layers -> (Int, Layers)
addLayer_ ls =
	(length $ layers ls,
		ls{layers = layers ls ++ [[]], buffed = buffed ls ++ [return ()]})

addLayerAction :: Layer -> (IO (), IO ()) -> IO ()
addLayerAction Layer{layerId = lid, layerLayers = rls} acts = do
	ls <- readIORef rls
	nls <- addLayerAction_ ls lid acts
	writeIORef rls nls

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
undoLayer Layer{layerId = lid, layerLayers = rls} = do
	ls <- readIORef rls
	mnls <- undoLayer_ ls lid
	maybe (return False) ((>> return True) . writeIORef rls) mnls

undoLayer_ :: Layers -> Int -> IO (Maybe Layers)
undoLayer_ ls l =
	if null $ layers ls !! l then return Nothing else do
		let nls = modifyAt (layers ls) l init
		undoLayersAction ls
		mapM_ snd $ concat nls
		return $ Just ls{layers = nls}

clearLayer :: Layer -> IO ()
clearLayer Layer{layerId = lid, layerLayers = rls} = do
	ls <- readIORef rls
	nls <- clearLayer_ ls lid
	writeIORef rls nls

clearLayer_ :: Layers -> Int -> IO Layers
clearLayer_ ls l = do
	let	nls = setAt (layers ls) l []
		nbf = setAt (buffed ls) l $ return ()
	clearLayersAction ls
	sequence_ nbf
	undoLayersAction ls
	mapM_ snd $ concat nls
	return ls{layers = nls, buffed = nbf}

addCharacter :: IORef Layers -> IO Character
addCharacter rls = do
	ls <- readIORef rls
	let (cid, nls) = addCharacter_ ls
	writeIORef rls nls
	return Character{characterId = cid, characterLayers = rls}

addCharacter_ :: Layers -> (Int, Layers)
addCharacter_ ls =
	(length $ characters ls,
		ls{characters = characters ls ++ [return ()]})

setCharacter :: Character -> IO () -> IO ()
setCharacter Character{characterId = cid, characterLayers = rls} act = do
	ls <- readIORef rls
	nls <- setCharacter_ ls cid act
	writeIORef rls nls

setCharacter_ :: Layers -> Int -> IO () -> IO Layers
setCharacter_ ls c act = do
	let cs = setAt (characters ls) c act
	clearCharactersAction ls
	sequence_ cs
	return ls{characters = cs}
