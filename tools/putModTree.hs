module Main where

import Text.RegexPR
import System.Directory
import Data.List
import Data.Tree
import System.Environment
import Control.Monad
import Control.Arrow(first)

main :: IO ()
main = do
	[ dir ] <- getArgs
	files <- fmap filterSource $ getDirectoryContentsRec dir
	dependList <- mapM ( depend dir files ) files
	let depList = map (first $ gsubRegexPR "/" ".") dependList
	mapM_ ( putStr . showTree [ ] . nubTree ) $
		let xs = mergeTree $ map makeTree depList in xs


getDirectoryContentsRec, getDirectoryContentsRecGen :: String -> IO [String]
getDirectoryContentsRec root = do
	pre <- getCurrentDirectory
	setCurrentDirectory root
	ret <- getDirectoryContentsRecGen "."
	setCurrentDirectory pre
	return $ map (tail . tail) ret
getDirectoryContentsRecGen root = do
	ps <- getDir root
	files <- filterM (doesFileExist . (root ++) . ("/" ++)) ps
	dirs <- filterM (doesDirectoryExist . (root ++) . ("/" ++)) ps
	subs <- fmap concat $ mapM (getDirectoryContentsRecGen . mkPath) dirs
	return $ map mkPath files ++ subs
--	roots <- fmap concat $ mapM (getDir . mkPath) dirs
	where
	mkPath = (root ++) . ("/" ++)
	getDir = fmap (filter $ flip notElem [".", ".."]) . getDirectoryContents

showTree :: [ Bool ] -> Tree String -> String
showTree n ( Node x ns ) =
	makePre ( reverse n ) ++ x ++ "\n" ++
		( concatMap ( showTree ( True : n ) ) ( init ns ) ++
		maybe "" ( showTree ( False : n ) ) ( last ns ) )
	where
	init [ ] = [ ]
	init [ x ] = [ ]
	init ( x : xs ) = x : init xs
	last [ ] = Nothing
	last [ x ] = Just x
	last ( _ : xs ) = last xs
	makePre [ ] = ""
	makePre [ _ ] = "  + "
	makePre ( True  : rest ) = "  | " ++ makePre rest
	makePre ( False : rest ) = "    " ++ makePre rest

nubTree :: Eq a => Tree a -> Tree a
nubTree ( Node x ns ) = Node x $ nub $ map nubTree ns

makeTree :: Eq a => ( a, [ a ] ) -> Tree a
makeTree ( x, xs ) = Node x $ map ( flip Node [ ] ) xs

mergeTree :: Eq a => [ Tree a ] -> [ Tree a ]
mergeTree ts = case map fst $ filter snd $ map ( `addTree_` ts ) ts of
	[ ]	-> ts
	new	-> mergeTree new

addTree_ :: Eq a => Tree a -> [ Tree a ] -> ( Tree a, Bool )
addTree_ t@( Node x _ ) ts = addTree t $ filter ( ( /= x ) . rootLabel ) ts

addTree :: Eq a => Tree a -> [ Tree a ] -> ( Tree a, Bool )
addTree ( Node x ns ) ts = case filter ( ( == x ) . rootLabel ) ts of
	[ ]	-> ( Node x $ map fst rets, any snd rets )
	t : _	-> ( t, True )
	where
	rets = map ( `addTree` ts ) ns

depend :: FilePath -> [ String ] -> String -> IO ( String, [ String ] )
depend dir fps fp = do
	cnt <- readAnyFile [ dir ++ "/" ++ fp ++ ".hs", dir ++ "/" ++ fp ++ ".y" ]
	return ( fp, map ( !! 1 ) $ ggetbrsRegexPR ( mkReg fps ) cnt )

filterSource :: [ FilePath ] -> [ FilePath ]
filterSource =
	map stripSuffix . filter ( isSuffixOf ".hs" ||| isSuffixOf ".y" ) .
	filter ( not . isPrefixOf "." )

mkReg :: [ FilePath ] -> String
mkReg fps_ =
	"^import\\s+(?:qualified\\s+)?(" ++ intercalate "|" fps ++ ")($|\\s|\\()"
	where
	fps = map (gsubRegexPR "/" ".") fps_

stripSuffix :: String -> String
stripSuffix = takeWhile ( /= '.' )

initN :: Int -> [ a ] -> [ a ]
initN n = ( !! n ) . iterate init

(|||) :: ( a -> Bool ) -> ( a -> Bool ) -> a -> Bool
( f1 ||| f2 ) x = f1 x || f2 x

readAnyFile :: [ FilePath ] -> IO String
readAnyFile ( f : fs ) = do
	ex <- doesFileExist f
	if ex then readFile f else readAnyFile fs
