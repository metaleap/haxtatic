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
} deriving (Eq, Show)


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
    let allfiles = concat<$> (Control.Monad.mapM perdir dirpaths)
        dirpaths = reldirs >~ (System.FilePath.combine rootdirpath)
        perfile :: FilePath -> IO (FilePath , Data.Time.Clock.UTCTime)
        perfile filepath =
            System.Directory.getModificationTime filepath >>= \ modtime
            -> return (filepath , modtime)
        perdir :: FilePath -> IO [(FilePath , Data.Time.Clock.UTCTime)]
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
                -> (Control.Monad.mapM perfile filepaths) >>= \filetuples
                -> (joinpath<$> dirs) >>= \subdirpaths
                -> (Control.Monad.mapM perdir subdirpaths) >>= \recursed
                -> return (concat [filetuples, concat recursed])
    in allfiles >>= \allfiletuples
    -> let tuple2tuple (fullpath , modtime) =
            (relpath , File { path = fullpath, content = "", modTime = modtime }) where
                relpath = Util.atOr filtered 0 fullpath
                filtered = filter Util.is (dirpaths>~persrcdir)
                persrcdir rd = if not (Util.startsWith fullpath rd)
                    then "" else drop (1+rd~>length) fullpath
        in return (allfiletuples>~tuple2tuple)



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
                writeTo filepath relpath defaultcontent
                >> return (File filepath defaultcontent (ctx~>nowTime))



rewrite file newmodtime newcontent =
    File {
        path = file~>path,
        content = newcontent, -- Util.fallback newcontent $content file,
        modTime = max newmodtime $modTime file
    }



writeTo filepath relpath filecontent =
    System.IO.hFlush System.IO.stdout
    >> System.Directory.createDirectoryIfMissing True (System.FilePath.takeDirectory filepath)
    >> putStr ("   >> "++relpath++"  [ ")
    >> System.IO.hFlush System.IO.stdout
    >> writeFile filepath filecontent
    >> System.IO.hFlush System.IO.stdout
    >> putStrLn "OK ]"
    >> System.IO.hFlush System.IO.stdout
