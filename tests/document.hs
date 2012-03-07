import Graphics.X11.Turtle
import Text.XML.YJSVG

leftSpace = 15

main = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
--	hideturtle t
	penup t
	setheading t 270
	let	w = 580
		h = 820
	goto t (- 90) (h / 2 - 20)
	write t "KochiMincho" 12 "はじめての TeX"
	goto t (w / 2 - 80) (h / 2 - 40)
	write t "KochiMincho" 12 "山田 太郎"
	goto t (- w / 2 + leftSpace + 10) (h / 2 - 60)
	write t "KochiMincho" 10 $ "これから TeX を勉強します。" ++
		"どのようにかいたらよいのかわかりません。"
	goto t (- w / 2 + leftSpace) (h / 2 - 72)
	pencolor t "blue"
	write t "KochiMincho" 10 $ "こんなものでよいのでしょうか。" ++
		"今、一番最初の段落を書いています。あまり、"
	goto t (- w / 2 + leftSpace) (h / 2 - 84)
	pencolor t "green"
	write t "KochiMincho" 10 $ "書くことがありません。"
	goto t (- w / 2 + leftSpace + 10) (h / 2 - 96)
	write t "KochiMincho" 10 $ "こんどは2番目の段落です。" ++
		"もうやめたくなってきました。"
	goto t (- w / 2 + leftSpace + 10) (h / 2 - 120)
	pencolor t "red"
	write t "KochiMincho" 10 "これで終わりにします。"
	waitField f
	getSVG t >>= putStr . showSVG w h
