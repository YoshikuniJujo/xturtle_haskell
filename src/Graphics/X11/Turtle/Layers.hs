module Graphics.X11.Turtle.Layers(
	-- * types
	Layers,
	Layer,
	Character,
	
	-- * initialize
	newLayers,
	makeLayer,
	makeCharacter,

	-- * draws
	redrawLayers,
	addDraw,
	setBackground,
	undoLayer,
	clearLayer,
	setCharacter
) where

import Control.Monad(when, unless)
import Data.IORef(IORef, newIORef, readIORef, atomicModifyIORef)
import Data.IORef.Tools(atomicModifyIORef_)
import Data.List.Tools(setAt, modifyAt)

--------------------------------------------------------------------------------

data Layers = Layers{
	undoNum :: Int,
	clearLayersAction :: IO (),
	undoLayersAction :: IO (),
	clearCharactersAction :: IO (),
	background :: [IO ()],
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
	background = [],
	buffed = [],
	layers = [],
	characters = []
 }

makeLayer :: IORef Layers -> IO Layer
makeLayer rls = atomicModifyIORef rls $ \ls -> (ls{
		layers = layers ls ++ [[]],
		buffed = buffed ls ++ [return ()],
		background = background ls ++[return ()]},
	Layer{layerId = length $ layers ls, layerLayers = rls})

makeCharacter :: IORef Layers -> IO Character
makeCharacter rls = atomicModifyIORef rls $ \ls ->
	(ls{characters = characters ls ++ [return ()]}, Character{
		characterId = length $ characters ls,
		characterLayers = rls})

--------------------------------------------------------------------------------

redrawLayers :: IORef Layers -> IO ()
redrawLayers rls = readIORef rls >>= \ls -> do
	sequence_ $ background ls
	clearLayersAction ls >> sequence_ (buffed ls)
	undoLayersAction ls >> mapM_ snd (concat $ layers ls)
	clearCharactersAction ls >> sequence_ (characters ls)

addDraw :: Layer -> (IO (), IO ()) -> IO ()
addDraw Layer{layerId = lid, layerLayers = rls} acts@(_, act) = do
	readIORef rls >>= \ls -> do
		act >> clearCharactersAction ls >> sequence_ (characters ls)
		unless (length (layers ls !! lid) < undoNum ls) $
			fst $ head $ layers ls !! lid
	atomicModifyIORef_ rls $ \ls -> if length (layers ls !! lid) < undoNum ls
		then ls{layers = modifyAt (layers ls) lid (++ [acts])}
		else let (a, _) : as = layers ls !! lid in ls{
			layers = setAt (layers ls) lid $ as ++ [acts],
			buffed = modifyAt (buffed ls) lid (>> a)}

setBackground :: Layer -> IO () -> IO ()
setBackground Layer{layerId = lid, layerLayers = rls} act = do
	atomicModifyIORef_ rls $ \ls ->
		ls{background = setAt (background ls) lid act}
	redrawLayers rls

undoLayer :: Layer -> IO Bool
undoLayer Layer{layerId = lid, layerLayers = rls} = do
	done <- atomicModifyIORef rls $ \ls -> if null $ layers ls !! lid
		then (ls, False)
		else (ls{layers = modifyAt (layers ls) lid init}, True)
	when done $ readIORef rls >>= \ls -> do
		undoLayersAction ls >> mapM_ snd (concat $ layers ls)
		clearCharactersAction ls >> sequence_ (characters ls)
	return done

clearLayer :: Layer -> IO ()
clearLayer Layer{layerId = lid, layerLayers = rls} = do
	atomicModifyIORef_ rls $ \ls -> ls{
		layers = setAt (layers ls) lid [],
		buffed = setAt (buffed ls) lid $ return ()}
	redrawLayers rls

setCharacter :: Character -> IO () -> IO ()
setCharacter Character{characterId = cid, characterLayers = rls} act = do
	atomicModifyIORef_ rls $ \ls ->
		ls{characters = setAt (characters ls) cid act}
	readIORef rls >>= \ls ->
		clearCharactersAction ls >> sequence_ (characters ls)
