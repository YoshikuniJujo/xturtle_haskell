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
	background,
	addDraw,
	undoLayer,
	clearLayer,
	character
) where

import Control.Monad(when, unless)
import Data.IORef(IORef, newIORef, readIORef, atomicModifyIORef)
import Data.IORef.Tools(atomicModifyIORef_)
import Data.List.Tools(setAt, modifyAt)

--------------------------------------------------------------------------------

data Layers = Layers{
	backgrounds :: [IO ()],
	buffers :: [IO ()],
	layers :: [[(IO (), IO ())]],
	characters :: [IO ()],
	buffSize :: Int,
	clearBuffers :: IO (),
	clearLayers :: IO (),
	clearCharacters :: IO ()}

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
newLayers bsize cbuf clyr cchr = newIORef Layers{
	backgrounds = [],
	buffers = [],
	layers = [],
	characters = [],
	buffSize = bsize,
	clearBuffers = cbuf,
	clearLayers = clyr,
	clearCharacters = cchr}

makeLayer :: IORef Layers -> IO Layer
makeLayer rls = atomicModifyIORef rls $ \ls -> (ls{
		backgrounds = backgrounds ls ++[return ()],
		buffers = buffers ls ++ [return ()],
		layers = layers ls ++ [[]]},
	Layer{layerId = length $ layers ls, layerLayers = rls})

makeCharacter :: IORef Layers -> IO Character
makeCharacter rls = atomicModifyIORef rls $ \ls ->
	(ls{characters = characters ls ++ [return ()]}, Character{
		characterId = length $ characters ls,
		characterLayers = rls})

--------------------------------------------------------------------------------

redrawLayers :: IORef Layers -> IO ()
redrawLayers rls = readIORef rls >>= \ls -> do
	clearBuffers ls >> sequence_ (backgrounds ls) >> sequence_ (buffers ls)
	clearLayers ls >> mapM_ snd (concat $ layers ls)
	clearCharacters ls >> sequence_ (characters ls)

addDraw :: Layer -> (IO (), IO ()) -> IO ()
addDraw Layer{layerId = lid, layerLayers = rls} acts@(_, act) = do
	readIORef rls >>= \ls -> do
		act >> clearCharacters ls >> sequence_ (characters ls)
		unless (length (layers ls !! lid) < buffSize ls) $
			fst $ head $ layers ls !! lid
	atomicModifyIORef_ rls $ \ls -> if length (layers ls !! lid) < buffSize ls
		then ls{layers = modifyAt (layers ls) lid (++ [acts])}
		else let (a, _) : as = layers ls !! lid in ls{
			layers = setAt (layers ls) lid $ as ++ [acts],
			buffers = modifyAt (buffers ls) lid (>> a)}

background, setBackground :: Layer -> IO () -> IO ()
background = setBackground
setBackground Layer{layerId = lid, layerLayers = rls} act = do
	atomicModifyIORef_ rls $ \ls ->
		ls{backgrounds = setAt (backgrounds ls) lid act}
	redrawLayers rls

undoLayer :: Layer -> IO Bool
undoLayer Layer{layerId = lid, layerLayers = rls} = do
	done <- atomicModifyIORef rls $ \ls -> if null $ layers ls !! lid
		then (ls, False)
		else (ls{layers = modifyAt (layers ls) lid init}, True)
	when done $ readIORef rls >>= \ls -> do
		clearLayers ls >> mapM_ snd (concat $ layers ls)
		clearCharacters ls >> sequence_ (characters ls)
	return done

clearLayer :: Layer -> IO ()
clearLayer Layer{layerId = lid, layerLayers = rls} = do
	atomicModifyIORef_ rls $ \ls -> ls{
		backgrounds = setAt (backgrounds ls) lid $ return (),
		layers = setAt (layers ls) lid [],
		buffers = setAt (buffers ls) lid $ return ()}
	redrawLayers rls

character, setCharacter :: Character -> IO () -> IO ()
character = setCharacter
setCharacter Character{characterId = cid, characterLayers = rls} act = do
	atomicModifyIORef_ rls $ \ls ->
		ls{characters = setAt (characters ls) cid act}
	readIORef rls >>= \ls ->
		clearCharacters ls >> sequence_ (characters ls)
