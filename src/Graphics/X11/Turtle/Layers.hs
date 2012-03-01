module Graphics.X11.Turtle.Layers(
	Layers,
	Layer,
	Character,
	
	newLayers,
	makeLayer,
	makeCharacter,

	addDraw,
	undoLayer,
	clearLayer,
	setCharacter,

	redrawLayers
) where

import Data.IORef(IORef, newIORef, readIORef, atomicModifyIORef)
import Data.List.Tools
import Control.Concurrent
import Control.Monad

data Layers = Layers{
	undoNum :: Int,
	undoLayersAction :: IO (),
	clearLayersAction :: IO (),
	clearCharactersAction :: IO (),
	buffed :: [IO ()],
	layers :: [[(IO (), IO ())]],
	characters :: [IO ()],
	lock :: Chan ()
 }

redrawLayers :: IORef Layers -> IO ()
redrawLayers rls = do
	ls <- readIORef rls
	clearLayerAction ls

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

makeLayer :: IORef Layers -> IO Layer
makeLayer = addLayer

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f =  atomicModifyIORef ref $ \x -> (f x, ())

addLayer :: IORef Layers -> IO Layer
addLayer rls = atomicModifyIORef rls $ \ls ->
	(ls{layers = layers ls ++ [[]], buffed = buffed ls ++ [return ()]},
		Layer{layerId = length $ layers ls, layerLayers = rls})

addDraw :: Layer -> (IO (), IO ()) -> IO ()
addDraw = addLayerAction

addLayerAction :: Layer -> (IO (), IO ()) -> IO ()
addLayerAction Layer{layerId = lid, layerLayers = rls} acts = do
	readIORef rls >>= \ls -> addLayerActionAction ls lid acts
	atomicModifyIORef_ rls $ \ls -> addLayerActionData ls lid acts

addLayerActionData :: Layers -> Int -> (IO (), IO ()) -> Layers
addLayerActionData ls l acts =
	let	actNum = length $ layers ls !! l
		nls1 = ls{layers = modifyAt (layers ls) l (++ [acts])}
		nls2 = ls{
			layers = modifyAt (layers ls) l ((++ [acts]) . tail),
			buffed = modifyAt (buffed ls) l
					(>> fst (head $ layers ls !! l))} in
	if actNum < undoNum ls then nls1 else nls2

addLayerActionAction :: Layers -> Int -> (IO (), IO ()) -> IO ()
addLayerActionAction ls l (_, act) = do
	let	actNum = length $ layers ls !! l
	act
	clearCharactersAction ls
	sequence_ $ characters ls
	unless (actNum < undoNum ls) $ fst $ head $ layers ls !! l

undoLayer :: Layer -> IO Bool
undoLayer Layer{layerId = lid, layerLayers = rls} = do
	ret <- atomicModifyIORef rls $ \ls -> case undoLayerData ls lid of
		Nothing -> (ls, False)
		Just nls -> (nls, True)
	when ret $ readIORef rls >>= \ls -> redrawFromUndo ls
	return ret

undoLayerData :: Layers -> Int -> Maybe Layers
undoLayerData ls l = if null $ layers ls !! l then Nothing
	else Just ls{layers = modifyAt (layers ls) l init}

clearLayer :: Layer -> IO ()
clearLayer Layer{layerId = lid, layerLayers = rls} = do
	atomicModifyIORef_ rls $ \ls -> clearLayerData ls lid
	readIORef rls >>= clearLayerAction

clearLayerData :: Layers -> Int -> Layers
clearLayerData ls l = ls{
	layers = setAt (layers ls) l [],
	buffed = setAt (buffed ls) l $ return ()
 }

clearLayerAction :: Layers -> IO ()
clearLayerAction ls = do
	clearLayersAction ls
	sequence_ $ buffed ls
	redrawFromUndo ls

makeCharacter :: IORef Layers -> IO Character
makeCharacter = addCharacter

addCharacter :: IORef Layers -> IO Character
addCharacter rls = atomicModifyIORef rls $ \ls ->
	let	(cid, nls) = addCharacter_ ls in
		(nls, Character{characterId = cid, characterLayers = rls})

addCharacter_ :: Layers -> (Int, Layers)
addCharacter_ ls =
	(length $ characters ls,
		ls{characters = characters ls ++ [return ()]})

setCharacter :: Character -> IO () -> IO ()
setCharacter Character{characterId = cid, characterLayers = rls} act = do
	atomicModifyIORef_ rls $ \ls -> setCharacterData ls cid act
	readIORef rls >>= setCharacterAction

setCharacterData :: Layers -> Int -> IO () -> Layers
setCharacterData ls c act = ls{characters = setAt (characters ls) c act}

setCharacterAction :: Layers -> IO ()
setCharacterAction ls = do
	clearCharactersAction ls
	sequence_ $ characters ls
