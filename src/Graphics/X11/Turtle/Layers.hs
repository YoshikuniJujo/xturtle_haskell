module Graphics.X11.Turtle.Layers(
	-- * types
	Layers,
	Layer,
	Character,
	
	-- * initilize
	newLayers,
	makeLayer,
	makeCharacter,

	-- * draws
	redrawLayers,
	addDraw,
	undoLayer,
	clearLayer,
	setCharacter
) where

import Control.Monad(when, unless)
import Data.IORef(IORef, newIORef, readIORef, atomicModifyIORef)
import Data.List.Tools(setAt, modifyAt)

--------------------------------------------------------------------------------

data Layers = Layers{
	undoNum :: Int,
	clearLayersAction :: IO (),
	undoLayersAction :: IO (),
	clearCharactersAction :: IO (),
	buffed :: [IO ()],
	layers :: [[(IO (), IO ())]],
	characters :: [IO ()]
 }

data Layer = Layer{
	layerId :: Int,
	layerLayers :: IORef Layers
 }

data Character = Character{
	characterId :: Int,
	characterLayers :: IORef Layers
 }

--------------------------------------------------------------------------------

newLayers :: Int -> IO () -> IO () -> IO () -> IO (IORef Layers)
newLayers un cla ula cca = newIORef Layers{
	undoNum = un,
	clearLayersAction = cla,
	undoLayersAction = ula,
	clearCharactersAction = cca,
	buffed = [],
	layers = [],
	characters = []
 }

makeLayer :: IORef Layers -> IO Layer
makeLayer rls = atomicModifyIORef rls $ \ls ->
	(ls{layers = layers ls ++ [[]], buffed = buffed ls ++ [return ()]},
		Layer{layerId = length $ layers ls, layerLayers = rls})

makeCharacter :: IORef Layers -> IO Character
makeCharacter rls = atomicModifyIORef rls $ \ls ->
	(ls{characters = characters ls ++ [return ()]}, Character{
		characterId = length $ characters ls,
		characterLayers = rls})

--------------------------------------------------------------------------------

redrawLayers :: IORef Layers -> IO ()
redrawLayers rls = readIORef rls >>= \ls -> do
	clearLayersAction ls
	sequence_ $ buffed ls
	redrawUndo ls

addDraw :: Layer -> (IO (), IO ()) -> IO ()
addDraw Layer{layerId = lid, layerLayers = rls} acts = do
	readIORef rls >>= \ls -> addDrawAction ls lid acts
	atomicModifyIORef_ rls $ \ls -> addDrawData ls lid acts

addDrawAction :: Layers -> Int -> (IO (), IO ()) -> IO ()
addDrawAction ls l (_, act) = do
	act >> clearCharactersAction ls >> sequence_ (characters ls)
	unless (length (layers ls !! l) < undoNum ls) $
		fst $ head $ layers ls !! l

addDrawData :: Layers -> Int -> (IO (), IO ()) -> Layers
addDrawData ls l acts = if length (layers ls !! l) < undoNum ls
	then ls{layers = modifyAt (layers ls) l (++ [acts])}
	else ls{layers = modifyAt (layers ls) l $ (++ [acts]) . tail,
		buffed = modifyAt (buffed ls) l (>> fst (head $ layers ls !! l))}

undoLayer :: Layer -> IO Bool
undoLayer Layer{layerId = lid, layerLayers = rls} = do
	ret <- atomicModifyIORef rls $ \ls -> if null $ layers ls !! lid
			then (ls, False)
			else (ls{layers = modifyAt (layers ls) lid init}, True)
	when ret $ readIORef rls >>= redrawUndo
	return ret

clearLayer :: Layer -> IO ()
clearLayer Layer{layerId = lid, layerLayers = rls} = do
	atomicModifyIORef_ rls $ \ls -> ls{
		layers = setAt (layers ls) lid [],
		buffed = setAt (buffed ls) lid $ return ()
	 }
	redrawLayers rls

setCharacter :: Character -> IO () -> IO ()
setCharacter Character{characterId = cid, characterLayers = rls} act = do
	atomicModifyIORef_ rls $ \ls ->
		ls{characters = setAt (characters ls) cid act}
	readIORef rls >>= \ls -> do
		clearCharactersAction ls
		sequence_ $ characters ls

redrawUndo :: Layers -> IO ()
redrawUndo ls = do
	undoLayersAction ls
	mapM_ snd $ concat $ layers ls
	clearCharactersAction ls
	sequence_ $ characters ls

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f =  atomicModifyIORef ref $ \x -> (f x, ())
