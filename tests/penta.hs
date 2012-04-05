import Graphics.X11.Turtle
import Control.Monad
import Text.XML.YJSVG

main :: IO ()
main = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	speed t "fastest"
	flushoff t
	left t 30
	pencolor t "gray"
	ps <- forM [5, 1, 3, 2, 4, 3] $ \x -> do
		goto t 0 0
		forward t $ x * 20
		left t 60
		position t
	beginfill t
	forM_ ps $ uncurry $ goto t
	endfill t
	goto t 0 0
--	pencolor t "black"
	pencolor t (0, 0, 0)
	forM_ ["やさしさ", "強さ", "卑劣さ", "自己愛", "ユーモア", "中二病"] $ \e -> do
		replicateM_ 10 $ do
			forward t 10
			left t 90
			forward t 3
			backward t 6
			forward t 3
			right t 90
		forward t 20
		penup t
		forward t 20
		write t "KochiMincho" 10 e
		goto t 0 0
		left t 60
		pendown t
	hideturtle t
	flush t
	waitField f
	w <- windowWidth t
	h <- windowHeight t
	getSVG t >>= putStrLn . showSVG w h
