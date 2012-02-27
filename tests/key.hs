import Graphics.X11.Turtle

main = do
	f <- openField
	onkeypress f $ \c -> do
		putChar c
		putChar '\n'
		return $ c /= 'q'
	waitField f
