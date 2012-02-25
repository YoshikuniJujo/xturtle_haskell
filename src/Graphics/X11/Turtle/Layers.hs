module Graphics.X11.Turtle.Layers(
) where

import System.IO.Unsafe
import Data.IORef
import Data.List.Tools
import Control.Monad

layersNum :: IORef Int
layersNum = unsafePerformIO $ newIORef 0

data Layers = Layers{
	layersId :: Int,
	undoNum :: Int,
	undoLayersAction :: IO (),
	clearLayersAction :: IO (),
	clearCharactersAction :: IO (),
	layers :: [[(IO (), IO ())]],
	characters :: [IO ()]
 }

data Layer = Layer{
	layerId :: Int,
	layerLayers :: Int
 }

data Character = Character{
	characterId :: Int,
	characterLayers :: Int
 }

addLayer :: Layers -> (Layer, Layers)
addLayer ls =
	(Layer{layerId = length $ layers ls, layerLayers = layersId ls},
		ls{layers = layers ls ++ [[]]})

addLayerAction :: Layers -> Layer -> (IO (), IO ()) -> IO Layers
addLayerAction ls l acts@(_, act) = do
	let actNum = length $ layers ls !! layerId l

	when (layersId ls /= layerLayers l) $ error "layer and layers not matched"
	act
	clearCharactersAction ls
	sequence_ $ characters ls
	return ls{layers = modifyAt (layers ls) (layerId l) (++ [acts])}

addCharacter :: Layers -> (Character, Layers)
addCharacter ls =
	(Character{characterId = length $ characters ls,
			characterLayers = layersId ls},
		ls{characters = characters ls ++ [return ()]})

setCharacter :: Layers -> Character -> IO () -> IO Layers
setCharacter ls c act = do
	let cs = setAt (characters ls) (characterId c) act
	clearCharactersAction ls
	sequence_ cs
	return ls{characters = cs}
