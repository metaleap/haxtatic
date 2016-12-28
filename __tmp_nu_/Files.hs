{-# OPTIONS_GHC -Wall #-}

module Files where

import qualified Util
import Util ( (~>) , (~.) , (~|) , (|~) , (>~) , (#) )

import qualified Control.Monad
import qualified Data.List
import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.FilePath
import System.FilePath ( (</>) )
import qualified System.IO



--  general project input file
data File = NoFile | File {
    path :: FilePath,
    content :: String,
    modTime :: Data.Time.Clock.UTCTime
} deriving (Eq)


data Ctx = Ctx {
    dirPath :: FilePath,
    nowTime :: Data.Time.Clock.UTCTime
}



_isfsnameok = not . (Data.List.isPrefixOf ".")



filesInDir dir =
    System.Directory.doesDirectoryExist dir >>= \isdir
    -> if not isdir then return [] else
        System.Directory.listDirectory dir >>= \names
        -> Control.Monad.filterM isfile (names~|_isfsnameok) where
            isfile = (dir</>) ~. System.Directory.doesFileExist >>= return



listAllFiles rootdirpath reldirs =
    let allfilepaths = concat<$> (Control.Monad.mapM perdir dirpaths)
        dirpaths = reldirs >~ (System.FilePath.combine rootdirpath)
        perdir :: FilePath -> IO [FilePath]
        perdir dirpath =
            let isfile = isfskind System.Directory.doesFileExist
                isdir = isfskind System.Directory.doesDirectoryExist
                isfskind test = (dirpath</>) ~. test >>= return
                joinpath n = n >>= return . (dirpath</>)
            in System.Directory.doesDirectoryExist dirpath >>= \direxists
            -> if not direxists then return [] else
                System.Directory.listDirectory dirpath >>= \names
                -> let  oknames = names~|_isfsnameok
                        files = Control.Monad.filterM isfile oknames
                        dirs = Control.Monad.filterM isdir oknames
                in (joinpath<$> files) >>= \filepaths
                -> (joinpath<$> dirs) >>= \subdirpaths
                -> (Control.Monad.mapM perdir subdirpaths) >>= \recursed
                -> return (concat [concat recursed,filepaths])
    in allfilepaths >>= \fullpaths
    -> let totuple fullpath = (fullpath,relpath) where
            relpath = Util.atOr filtered 0 fullpath
            filtered = filter Util.is (dirpaths>~persrcdir)
            persrcdir rd = if not (Util.startsWith fullpath rd)
                then "" else drop (1+rd~>length) fullpath
        in return (fullpaths>~totuple)



readOrCreate ctx relpath relpath2 defaultcontent =
    if null relpath then return NoFile else
    let filepath = System.FilePath.combine (ctx~>dirPath) relpath
        filepath2 = System.FilePath.combine (ctx~>dirPath) relpath2
    in System.Directory.doesFileExist filepath
    >>= \ isfile -> if isfile
        then
            System.Directory.getModificationTime filepath >>= \ modtime
            -> readFile filepath >>= \ filecontent
            -> return (File filepath filecontent modtime)
        else if Util.is relpath2
            then readOrCreate ctx relpath2 "" defaultcontent
            else
                System.Directory.createDirectoryIfMissing True (System.FilePath.takeDirectory filepath)
                >> writeTo False filepath relpath defaultcontent
                >> return (File filepath defaultcontent (ctx~>nowTime))



rewrite file newmodtime newcontent =
    File {
        path = file~>path,
        content = newcontent, -- Util.fallback newcontent $content file,
        modTime = max newmodtime $modTime file
    }



writeTo onlyifnofilesindir filepath relpath filecontent =
    let
        dirfiles = if onlyifnofilesindir
                    then filesInDir (System.FilePath.takeDirectory filepath)
                    else return []
    in
        dirfiles >>= \names
        -> if onlyifnofilesindir && Util.is names
            then System.IO.hFlush System.IO.stdout
            else
                System.IO.hFlush System.IO.stdout
                >> putStr ("   >> "++relpath++"  [ ")
                >> System.IO.hFlush System.IO.stdout
                >> writeFile filepath filecontent
                >> System.IO.hFlush System.IO.stdout
                >> putStrLn "OK ]"
                >> System.IO.hFlush System.IO.stdout
