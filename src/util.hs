module Util where

import Control.Monad
import Data.Char
import Data.List
import Data.List.Utils
import System.Directory
import System.FilePath


type FName = [String]
fnYear (y:_) = y
fnMonth (_:m:_) = m
fnDay (_:_:d:_) = d
fnName (_:_:_:n:_) = n



type KeyVals = [(String,String)]
keyVal [] _ def = def
keyVal ((k,v):kvt) key def
    | key==k = v
    | null kvt = def
    | otherwise = keyVal kvt key def
keyValApp [] _ def _ = def
keyValApp ((k,v):kvt) key def app
    | key==k = v++app
    | null kvt = def
    | otherwise = keyValApp kvt key def app


-- readability-sugar helpers
iif True dis _ = dis ; iif False _ dat = dat
is [] = False ; is _ = True
isin _ [] = False
isin v (lh:lt) = (v == lh) || isin v lt
count p [] = 0
count p (lh:lt) = (b2i $ p lh)+(count p lt) where b2i True = 1 ; b2i False = 0
indexif p [] = minBound::Int
indexif p (lh:lt) = if (p lh) then 0 else 1+(indexif p lt)
indexof v = (indexif . (==)) v
within minval maxval val = val>=minval && val<=maxval
whilein l i p next def
    | (i<0) || (i>=(length l)) = def
    | (p v) = whilein l (next i) p next def
    | otherwise = v
    where v = l!!i
swapargs fn x y = fn y x
readInt s = read s :: Int
drop3 = drop 3 ; take3 = take 3
join = Data.List.intercalate
trimChar ch = Data.List.dropWhile ((==) ch)
trimSpace = Data.List.dropWhile Data.Char.isSpace


-- the Phil standard library..
replacein str [] = str
replacein str ((old,new):rest) =
    Data.List.Utils.replace old new (replacein str rest)
swapout old new = map (\item -> if (item==old) then new else item)
splitBy delim = foldr per_elem [[]] where
    per_elem el elems@(first:rest) | (el==delim) = []:elems | otherwise = (el:first):rest
quickSort prop less greater = sorted where
    sorted [] = []
    sorted (el:rest) = (sorted $ cmp less) ++ [el] ++ (sorted $ cmp greater) where
        cmp op = [e | e <- rest, op (prop e) (prop el)]


-- copy all files & folders within srcdir into dstdir
copyDirTree srcdir dstdir =
    System.Directory.createDirectoryIfMissing True dstdir >>
        System.Directory.getDirectoryContents srcdir >>=
            copyAll srcdir dstdir True


-- copy all files & folders (named in fsnames) within srcdir into dstdir
copyAll srcdir dstdir dofolders fsnames =
    mapM_ per_fsname fsnames where
        per_fsname "." = return () ; per_fsname ".." = return ()
        per_fsname fsname = doesFileExist srcpath >>= \isfile ->
            (if isfile then (System.Directory.copyFile srcpath dstpath) else if dofolders then (copyDirTree  srcpath dstpath) else return ())
                where srcpath = srcdir </> fsname ; dstpath = dstdir </> fsname

getAllFiles rootdir curdir = let curpath = rootdir </> curdir in do
    names <- getDirectoryContents curpath
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = curpath </> name
        isDirectory <- doesDirectoryExist path
        let bla = curdir </> name in
            if isDirectory then getAllFiles rootdir bla else return [bla]
    return (concat paths)
