{-# OPTIONS_GHC -Wall #-}

module Files where

import qualified Util
import Util ( noNull , (~:) , (~.) , (~|) , (>~) , (>>~) , (>>|) )

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
    curDir :: FilePath,
    dirPath :: FilePath,
    nowTime :: Data.Time.Clock.UTCTime
}



_isfsnameok = not . (Data.List.isPrefixOf ".")



copyTo _ [] = return ()
copyTo srcfilepath ("":dstmore) = copyTo srcfilepath dstmore
copyTo srcfilepath (dstpath:dstmore) =
    System.Directory.createDirectoryIfMissing True (System.FilePath.takeDirectory dstpath)
    >> System.Directory.copyFile srcfilepath dstpath
    >> copyTo srcfilepath dstmore



ensureFileExt filepath "" = filepath
ensureFileExt "" ext = ext
ensureFileExt filepath ext =
    let curext = System.FilePath.takeExtension filepath
    in if curext==ext then filepath else filepath++ext



filesInDir dir =
    System.Directory.doesDirectoryExist dir >>= \isdir
    -> if not isdir then return [] else
        System.Directory.listDirectory dir >>= \names
        -> (names~|_isfsnameok) >>| isfile where
            isfile = (dir</>) ~. System.Directory.doesFileExist >>= return



listAllFiles rootdirpath reldirs permodtime =
    let allfiles = concat<$> (dirpaths >>~ foreachdir)
        dirpaths = reldirs >~ (System.FilePath.combine rootdirpath)
        foreachfile :: FilePath -> IO (FilePath , Data.Time.Clock.UTCTime)
        foreachfile filepath =
            System.Directory.getModificationTime filepath >>= \ modtime
            -> return (filepath , permodtime modtime)
        foreachdir :: FilePath -> IO [(FilePath , Data.Time.Clock.UTCTime)]
        foreachdir dirpath =
            let fstest test = (dirpath</>) ~. test >>= return
                joinpath n = n >>= return . (dirpath</>)
            in System.Directory.doesDirectoryExist dirpath >>= \direxists
            -> if not direxists then return [] else
                System.Directory.listDirectory dirpath >>= \names
                -> let  oknames = names ~| _isfsnameok
                        files = oknames >>| fstest System.Directory.doesFileExist
                        dirs = oknames >>| fstest System.Directory.doesDirectoryExist
                in (joinpath<$> files) >>= \filepaths
                -> (filepaths >>~ foreachfile) >>= \filetuples
                -> (joinpath<$> dirs) >>= \subdirpaths
                -> (subdirpaths >>~ foreachdir) >>= \recursed
                -> return (concat [filetuples, concat recursed])
    in allfiles >>= \allfiletuples
    -> let foreachfiletuple (srcfilepath , modtime) =
            ( Util.atOr relpaths 0 srcfilepath,
                File { path = srcfilepath, content = "", modTime = modtime } )
            where
            relpaths = dirpaths>~foreachsrcdir ~|noNull
            foreachsrcdir reldirpath =
                if Util.startsWith srcfilepath reldirpath then
                    drop (1+reldirpath~:length) srcfilepath else ""
        in return$ allfiletuples>~foreachfiletuple



pathSepSlashToSystem =
    Util.substitute '/' System.FilePath.pathSeparator

pathSepSystemToSlash =
    --  no System.FilePath.pathSeparator: for data from windows users being used on posix
    Util.substitute '\\' '/'



readOrCreate ctx relpath relpath2 defaultcontent =
    if null relpath then return NoFile else
    let filepath = System.FilePath.combine (ctx~:dirPath) relpath
    in System.Directory.doesFileExist filepath
    >>= \ isfile -> if isfile
        then
            System.Directory.getModificationTime filepath >>= \ modtime
            -> readFile filepath >>= \ filecontent
            -> return (File filepath filecontent modtime)
        else if relpath2~:noNull
            then readOrCreate ctx relpath2 "" defaultcontent
            else
                writeTo filepath relpath defaultcontent
                >> return (File filepath defaultcontent (ctx~:nowTime))



rewrite file newmodtime newcontent =
    File {
        path = file~:path,
        content = newcontent, -- Util.ifNull newcontent $content file,
        modTime = max newmodtime $modTime file
    }



simpleFileNameMatch =
    simpleFilePathMatch . System.FilePath.takeFileName

simpleFileNameMatchAny =
    simpleFilePathMatchAny . System.FilePath.takeFileName

simpleFilePathMatch _ "*" = True
simpleFilePathMatch relpath dumbpattern =
    let testcontains = patternstarts && patternends
        teststarts = (not testcontains) && patternends
        testends = (not testcontains) && patternstarts
        patternstarts = Util.startsWith dumbpattern "*"
        patternends = Util.endsWith dumbpattern "*"
    in (testcontains && Util.contains relpath (Util.crop 1 1 dumbpattern))
    || (teststarts && Util.startsWith relpath (Util.crop 0 1 dumbpattern))
    || (testends && Util.endsWith relpath (Util.crop 1 0 dumbpattern))

simpleFilePathMatchAny relpath dumbpatterns =
    or$ dumbpatterns >~ (simpleFilePathMatch relpath)



writeTo filepath showpath filecontent =
    System.IO.hFlush System.IO.stdout
    >> System.Directory.createDirectoryIfMissing True (System.FilePath.takeDirectory filepath)
    >> putStr ("   >> "++showpath++"  [ ")
    >> System.IO.hFlush System.IO.stdout
    >> writeFile filepath filecontent
    >> System.IO.hFlush System.IO.stdout
    >> putStrLn "OK ]"
    >> System.IO.hFlush System.IO.stdout
