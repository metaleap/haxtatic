module Util where

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
keyVal [] _ def _ = def
keyVal ((k,v):kvt) key def append
    | key==k = v++append
    | null kvt = def
    | otherwise = keyVal kvt key def append


-- readability-sugar helpers
when True dis _ = dis ; when False _ dat = dat
whennot True _ dat = dat ; whennot False dis _ = dis
is [] = False ; is _ = True
isin _ [] = False
isin v (lh:lt) = (v == lh) || isin v lt
swapargs fn x y = fn y x
readInt s = read s :: Int
drop3 = drop 3 ; take3 = take 3
join = Data.List.intercalate


-- the Phil standard library..
fsPath dir = (++) (dir++[System.FilePath.pathSeparator])
replace str [] = str
replace str ((old,new):rest) =
    Data.List.Utils.replace old new (Util.replace str rest)
swapout old new = map (\item -> when (item==old) new item)
splitBy delim = foldr per_elem [[]] where
    per_elem el elems@(first:rest) | (el==delim) = []:elems | otherwise = (el:first):rest
quickSort prop less greater = sorted where
    sorted [] = []
    sorted (el:rest) = (sorted $ cmp less) ++ [el] ++ (sorted $ cmp greater) where
        cmp op = [e | e <- rest, op (prop e) (prop el)]


monthNames = ["","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
monthName m = monthNames!!(readInt m)


-- copy all files & folders within srcdir into dstdir
copyDirTree srcdir dstdir =
    System.Directory.createDirectoryIfMissing True dstdir >>
        System.Directory.getDirectoryContents srcdir >>=
            copyFiles srcdir dstdir


-- copy all files & folders (named in fsnames) within srcdir into dstdir
copyFiles srcdir dstdir fsnames =
    mapM_ per_fsname fsnames where
        per_fsname "." = return () ; per_fsname ".." = return ()
        per_fsname fsname =
            doesFileExist srcpath >>= \isfile -> (when isfile System.Directory.copyFile copyDirTree) srcpath dstpath
                where srcpath = fsPath srcdir fsname ; dstpath = fsPath dstdir fsname
