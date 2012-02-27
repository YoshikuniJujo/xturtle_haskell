module Graphics.X11.Turtle.Layers(
	Layers,
	LayerRef,
	CharacterRef,
	
	newLayersRef,
	addLayerRef,
	addCharacterRef,

	addLayerActionRef,
	undoLayerRef,
	clearLayerRef,
	setCharacterRef,
) where

import System.IO.Unsafe
import Data.IORef
import Data.List.Tools

layersNum :: IORef Int
layersNum = unsafePerformIO $ newIORef 0

data Layers = Layers{
	layersId :: Int,
	undoNum :: Int,
	undoLayersAction :: IO (),
	clearLayersAction :: IO (),
	clearCharactersAction :: IO (),
	buffed :: [IO ()],
	layers :: [[(IO (), IO ())]],
	characters :: [IO ()]
 }

newLayersRef :: Int -> IO () -> IO () -> IO () -> IO (IORef Layers)
newLayersRef un ula cla cca = do
	ls <- newLayers un ula cla cca
	newIORef ls

newLayers :: Int -> IO () -> IO () -> IO () -> IO Layers
newLayers un ula cla cca = do
	i <- readIORef layersNum
	writeIORef layersNum (i + 1)
	return Layers{
		layersId = i,
		undoNum = un,
		undoLayersAction = ula,
		clearLayersAction = cla,
		clearCharactersAction = cca,
		buffed = [],
		layers = [],
		characters = []
	 }

data LayerRef = LayerRef{
	layerRefId :: Int,
	layerLayersRef :: IORef Layers
 }

data CharacterRef = CharacterRef{
	characterRefId :: Int,
	characterLayersRef :: IORef Layers
 }

data Layer = Layer{
	layerId :: Int
--	layerLayers :: Int
 }

data Character = Character{
	characterId :: Int
--	characterLayers :: Int
 }

addLayerRef :: IORef Layers -> IO LayerRef
addLayerRef rls = do
	ls <- readIORef rls
	let	(Layer{layerId = lid}, nls) = addLayer ls
	writeIORef rls nls
	return LayerRef{layerRefId = lid, layerLayersRef = rls}

addLayer :: Layers -> (Layer, Layers)
addLayer ls =
	(Layer{layerId = length $ layers ls}, -- , layerLayers = layersId ls},
		ls{layers = layers ls ++ [[]], buffed = buffed ls ++ [return ()]})

addLayerActionRef :: LayerRef -> (IO (), IO ()) -> IO ()
addLayerActionRef LayerRef{layerRefId = lid, layerLayersRef = rls} acts = do
	ls <- readIORef rls
	nls <- addLayerAction ls Layer{layerId = lid} acts
	writeIORef rls nls

addLayerAction :: Layers -> Layer -> (IO (), IO ()) -> IO Layers
addLayerAction ls l acts@(_, act) = do
--	when (layersId ls /= layerLayers l) $ error "layer and layers not matched"
	let actNum = length $ layers ls !! layerId l
	act
	clearCharactersAction ls
	sequence_ $ characters ls
	if actNum < undoNum ls then
			return ls{layers =
				modifyAt (layers ls) (layerId l) (++ [acts])}
		else do	fst $ head $ layers ls !! layerId l
			return ls{
				layers = modifyAt (layers ls) (layerId l)
					((++ [acts]) . tail),
				buffed = modifyAt (buffed ls) (layerId l)
					(>> fst (head $ layers ls !! layerId l))}

undoLayerRef :: LayerRef -> IO Bool
undoLayerRef LayerRef{layerRefId = lid, layerLayersRef = rls} = do
	ls <- readIORef rls
	mnls <- undoLayer ls Layer{layerId = lid}
	maybe (return False) ((>> return True) . writeIORef rls) mnls

undoLayer :: Layers -> Layer -> IO (Maybe Layers)
undoLayer ls l =
	if null $ layers ls !! layerId l then return Nothing else do
		let nls = modifyAt (layers ls) (layerId l) init
		undoLayersAction ls
		mapM_ snd $ concat nls
		return $ Just ls{layers = nls}

clearLayerRef :: LayerRef -> IO ()
clearLayerRef LayerRef{layerRefId = lid, layerLayersRef = rls} = do
	ls <- readIORef rls
	nls <- clearLayer ls Layer{layerId = lid}
	writeIORef rls nls

clearLayer :: Layers -> Layer -> IO Layers
clearLayer ls l = do
--	when (layersId ls /= layerLayers l) $ error "layer and layers not matched"
	let	nls = setAt (layers ls) (layerId l) []
		nbf = setAt (buffed ls) (layerId l) $ return ()
	clearLayersAction ls
	sequence_ nbf
	undoLayersAction ls
	mapM_ snd $ concat nls
	return ls{layers = nls, buffed = nbf}

addCharacterRef :: IORef Layers -> IO CharacterRef
addCharacterRef rls = do
	ls <- readIORef rls
	let (Character{characterId = cid}, nls) = addCharacter ls
	writeIORef rls nls
	return CharacterRef{characterRefId = cid, characterLayersRef = rls}

addCharacter :: Layers -> (Character, Layers)
addCharacter ls =
	(Character{characterId = length $ characters ls}, -- ,
--			characterLayers = layersId ls},
		ls{characters = characters ls ++ [return ()]})

setCharacterRef :: CharacterRef -> IO () -> IO ()
setCharacterRef CharacterRef{characterRefId = cid, characterLayersRef = rls} act = do
	ls <- readIORef rls
	nls <- setCharacter ls Character{characterId = cid} act
	writeIORef rls nls

setCharacter :: Layers -> Character -> IO () -> IO Layers
setCharacter ls c act = do
--	when (layersId ls /= characterLayers c) $ error "character and layers not matched"
	let cs = setAt (characters ls) (characterId c) act
	clearCharactersAction ls
	sequence_ cs
	return ls{characters = cs}
