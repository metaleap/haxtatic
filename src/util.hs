{-# OPTIONS_GHC -Wall #-}
module Util where


import qualified Control.Monad
import qualified Data.Char
import qualified Data.List
import qualified Data.List.Utils
import qualified Data.Time.Clock
import qualified System.Directory
import System.FilePath ( (</>) )



type FName = [String]

fnYear ::
    FName->
    String
fnYear (y:_) = y ; fnYear _ = ""
fnMonth ::
    FName->
    String
fnMonth (_:m:_) = m ; fnMonth _ = ""
fnDay ::
    FName->
    String
fnDay (_:_:d:_) = d ; fnDay _ = ""
fnName ::
    FName->
    String
fnName (_:_:_:n:_) = n ; fnName _ = ""



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
mergeKeyVals kvdefaults kvoverwrites =
    Data.List.nub $ (map overwrite kvdefaults)++kvoverwrites where
        overwrite (k,v) = (k, keyVal kvoverwrites k v)



-- readability-sugar helpers
is v = (not . null) v
count _ [] = 0
count p (lh:lt) = if (p lh) then 1+count' else count' where
    count' = count p lt
indexOf v = (indexIf . (==)) v
indexIf _ [] = minBound::Int
indexIf p (lh:lt) = if (p lh) then 0 else 1+(indexIf p lt)
within minval maxval val = val>=minval && val<=maxval
whileIn l i p next def
    | (i<0) || (i>=(length l)) = def
    | (p v) = whileIn l (next i) p next def
    | otherwise = v
    where v = l!!i
swapArgs fn x y = fn y x
via fn = ((>>) fn) . return
drop3 = drop 3 ; take3 = take 3
join = Data.List.intercalate
trimChar ch = Data.List.dropWhile ((==) ch)

readInt::
    String->
    Int
readInt s = read s :: Int

trimSpace :: String->String
trimSpace = Data.List.dropWhile Data.Char.isSpace

since :: Data.Time.Clock.UTCTime->Data.Time.Clock.UTCTime->Data.Time.Clock.NominalDiffTime
since = swapArgs Data.Time.Clock.diffUTCTime



indexOfSub _ [] = minBound::Int
indexOfSub sub str@(_:rest)
    | all (uncurry (==)) (zip sub str)
        = 0
    | otherwise
        = 1+(indexOfSub sub rest)

lastIndexOfSub rev sub str
    | idx<0 = -1
    | otherwise = (length str)-(length sub)-idx
    where idx = indexOfSub (rev sub) (rev str)


splitUp::
    [String]-> String-> String->
    [(String,String)]
splitUp [] _ _ = []
splitUp _ "" _ = []
splitUp _ _ "" = []
splitUp begins end str =
    (lst pre "") ++ (lst match beg) ++
        --  only recurse if we have a good reason:
        (if nomatch && splitat==0 then (lst rest "") else (splitUp begins end rest))
    where
        pre = if nomatch then take splitat str else take bpos str
        match = if nomatch then "" else drop (bpos+blen) $ take epos str
        rest = if nomatch then drop splitat str else drop eposlen str
        beg = if nomatch then "" else take blen $ drop bpos $ take epos str
        nomatch = epos<0 || bpos<0
        splitat = if nomatch && epos>=0 then eposlen else 0
        epos = indexOfSub end str
        bpos = if epos<0 then -1 else let bstr = reverse $ take epos str in
            maximum $ map (\b -> lastIndexOfSub id b bstr) begins
        eposlen = epos+(length end) ; blen = length (begins!!0)
        lst s b = if null s then [] else [(s,b)]


-- the Phil standard library..
replaceIn str [] = str
replaceIn str ((old,new):rest) =
    Data.List.Utils.replace old new (replaceIn str rest)
substitute old new = map (\item -> if (item==old) then new else item)
splitBy delim = foldr per_elem [[]] where
    per_elem _ [] = []
    per_elem el elems@(first:rest)
        | (el==delim) = []:elems
        | otherwise = (el:first):rest
quickSort prop less greater = sorted where
    sorted [] = []
    sorted (el:rest) = (sorted $ cmp less) ++ [el] ++ (sorted $ cmp greater) where
        cmp op = [e | e <- rest, op (prop e) (prop el)]


-- copy all files & folders within srcdir into dstdir
copyDirTree::
    FilePath-> FilePath->
    IO ()
copyDirTree srcdir dstdir =
    System.Directory.createDirectoryIfMissing True dstdir
    >> System.Directory.getDirectoryContents srcdir
        >>= copyAll srcdir dstdir True



-- copy all files & folders (named in fsnames) within srcdir into dstdir
copyAll::
    FilePath-> FilePath-> Bool-> [FilePath]->
    IO ()
copyAll srcdir dstdir dofolders fsnames =
    mapM_ per_fsname fsnames where
        per_fsname "." = return () ; per_fsname ".." = return ()
        per_fsname fsname = System.Directory.doesFileExist srcpath >>= \isfile ->
            (if isfile then (System.Directory.copyFile srcpath dstpath) else if dofolders then (copyDirTree  srcpath dstpath) else return ())
                where srcpath = srcdir </> fsname ; dstpath = dstdir </> fsname



getAllFiles::
    FilePath-> FilePath->
    IO [FilePath]
getAllFiles rootdir curdir =
    let curpath = rootdir </> curdir ; validnames = filter $ not . (Data.List.isPrefixOf ".")
        pername name =
            System.Directory.doesDirectoryExist (curpath </> name)
            >>= walkortalk (curdir </> name) where
                walkortalk relpath isdir = (if isdir then (getAllFiles rootdir) else (return . l1)) relpath ; l1 v = [v]
    in
        System.Directory.getDirectoryContents curpath
        >>= (swapArgs Control.Monad.forM pername) . validnames
        >>= (return . concat . (Util.quickSort length (<) (>=)))
        -- why sort: want top-level files first. hacky but works for the very cases where it makes a difference
