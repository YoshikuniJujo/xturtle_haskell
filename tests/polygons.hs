import Graphics.X11.Turtle
import Control.Monad

star10 :: Turtle -> IO ()
star10 t = replicateM_ 10 $ forward t 100 >> left t 108

star100 :: Turtle -> IO ()
star100 t = replicateM_ 100 $ forward t 100 >> left t 183.6
