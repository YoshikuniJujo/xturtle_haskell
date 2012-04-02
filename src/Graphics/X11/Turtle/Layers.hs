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
	bgs :: [IO ()], buffs :: [IO ()], layers :: [[(IO (), IO ())]],
	chars :: [IO ()], buffSize :: Int,
	clearBuffers :: IO (), clearLayers :: IO (), clearCharacters :: IO ()}

data Layer = Layer{layerId :: Int, layerLayers :: IORef Layers}
data Character = Character{charId :: Int, charLayers :: IORef Layers}

--------------------------------------------------------------------------------

newLayers :: Int -> IO () -> IO () -> IO () -> IO (IORef Layers)
newLayers bsize cbuf clyr cchr = newIORef Layers{
	bgs = [], buffs = [], layers = [], chars = [], buffSize = bsize,
	clearBuffers = cbuf, clearLayers = clyr, clearCharacters = cchr}

makeLayer :: IORef Layers -> IO Layer
makeLayer rls = atomicModifyIORef rls $ \ls -> (ls{
		bgs = bgs ls ++[return ()], buffs = buffs ls ++ [return ()],
		layers = layers ls ++ [[]]},
	Layer{layerId = length $ layers ls, layerLayers = rls})

makeCharacter :: IORef Layers -> IO Character
makeCharacter rls = atomicModifyIORef rls $ \ls -> (ls{
		chars = chars ls ++ [return ()]},
	Character{charId = length $ chars ls, charLayers = rls})

--------------------------------------------------------------------------------

redrawLayers :: IORef Layers -> IO ()
redrawLayers rls = readIORef rls >>= \ls -> do
	clearBuffers ls >> sequence_ (bgs ls) >> sequence_ (buffs ls)
	clearLayers ls >> mapM_ snd (concat $ layers ls)
	clearCharacters ls >> sequence_ (chars ls)

background :: Layer -> IO () -> IO ()
background Layer{layerId = lid, layerLayers = rls} act =
	atomicModifyIORef_ rls (\ls -> ls{bgs = setAt (bgs ls) lid act})
		>> redrawLayers rls

addDraw :: Layer -> (IO (), IO ()) -> IO ()
addDraw Layer{layerId = lid, layerLayers = rls} acts@(_, act) = do
	readIORef rls >>= \ls -> do
		act >> clearCharacters ls >> sequence_ (chars ls)
		unless (length (layers ls !! lid) < buffSize ls) $
			fst $ head $ layers ls !! lid
	atomicModifyIORef_ rls $ \ls ->
		if length (layers ls !! lid) < buffSize ls
			then ls{layers = modifyAt (layers ls) lid (++ [acts])}
			else let (a, _) : as = layers ls !! lid in ls{
				layers = setAt (layers ls) lid $ as ++ [acts],
				buffs = modifyAt (buffs ls) lid (>> a)}

undoLayer :: Layer -> IO Bool
undoLayer Layer{layerId = lid, layerLayers = rls} = do
	done <- atomicModifyIORef rls $ \ls -> if null $ layers ls !! lid
		then (ls, False)
		else (ls{layers = modifyAt (layers ls) lid init}, True)
	when done $ readIORef rls >>= \ls -> do
		clearLayers ls >> mapM_ snd (concat $ layers ls)
		clearCharacters ls >> sequence_ (chars ls)
	return done

clearLayer :: Layer -> IO ()
clearLayer Layer{layerId = lid, layerLayers = rls} =
	atomicModifyIORef_ rls (\ls -> ls{
		bgs = setAt (bgs ls) lid $ return (),
		buffs = setAt (buffs ls) lid $ return (),
		layers = setAt (layers ls) lid []}) >> redrawLayers rls

character :: Character -> IO () -> IO ()
character Character{charId = cid, charLayers = rls} act = do
	atomicModifyIORef_ rls $ \ls -> ls{chars = setAt (chars ls) cid act}
	readIORef rls >>= \ls -> clearCharacters ls >> sequence_ (chars ls)
