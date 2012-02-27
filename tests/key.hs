import Graphics.X11.Turtle

main = do
	f <- openField
	onkeypress f $ \c -> do
		print c
		return $ c /= 'q'
	waitField f
