{-# OPTIONS_GHC -Wall #-}

module Files where

import qualified Util
import Util ( noNull , (~:) , (~.) , (~|) , (|~) , (>~) , (>>~) , (>>|) )

import qualified Data.List
import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.FilePath
import System.FilePath ( (</>) )
import qualified System.IO
import qualified Control.Monad

--  general project input file
data File
    = NoFile
    | FileInfo {    path :: FilePath,
                    modTime :: Data.Time.Clock.UTCTime }
    | FileFull {    path :: FilePath,
                    modTime :: Data.Time.Clock.UTCTime,
                    content :: String }
    deriving (Eq)


data Ctx = AppContext {
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



customDateFromFileName dateparser (filepath , srcfile) =
    let
        (filedir , filename) = System.FilePath.splitFileName filepath
        (datepart , fnrest) = System.FilePath.splitExtensions filename
        datemaybe = dateparser datepart
    in case datemaybe of
        Just customdate->
            ( (saneDirPath filedir) </> (Util.trimStart' ['.'] fnrest) , customdate )
        Nothing->
            ( filepath , srcfile~:modTime )



ensureFileExt _ "" filepath = filepath
ensureFileExt _ _ "" = ""
ensureFileExt ignorecase ext filepath =
    let curext = System.FilePath.takeExtension filepath
        cmp = if ignorecase then Util.toLower else id
    in if (cmp curext)==(cmp ext)
        then filepath else filepath++ext



filesInDir dir =
    System.Directory.doesDirectoryExist dir >>= ifexists where
        ifexists False =
            return []
        ifexists True =
            System.Directory.listDirectory dir >>=
                (~| _isfsnameok) ~. (>>| filesonly)
        filesonly = (dir</>) ~. System.Directory.doesFileExist



fullFrom oldfile cmpmodtime newcontent =
    FileFull (oldfile~:path) (max cmpmodtime $oldfile~:modTime) newcontent



listAllFiles rootdirpath reldirs modtimer =
    let allfiles = concat<$> (dirpaths >>~ foreachdir)
        dirpaths = reldirs >~ (System.FilePath.combine rootdirpath)
        foreachfile :: FilePath -> IO (FilePath , Data.Time.Clock.UTCTime)
        foreachfile filepath =
            System.Directory.getModificationTime filepath >>= \ modtime
            -> return (filepath , modtimer modtime)
        foreachdir :: FilePath -> IO [(FilePath , Data.Time.Clock.UTCTime)]
        foreachdir dirpath =
            System.Directory.doesDirectoryExist dirpath >>= ifexists where
                ifexists False =
                    return []
                ifexists True =
                    System.Directory.listDirectory dirpath >>= \names
                    -> let  oknames = names ~| _isfsnameok
                            files = oknames >>| fstest System.Directory.doesFileExist
                            dirs = oknames >>| fstest System.Directory.doesDirectoryExist
                            fstest = ((dirpath</>) ~.)
                            joinpath n = n >>= return.(dirpath</>)
                    in (joinpath<$> files) >>= \filepaths
                    -> (filepaths >>~ foreachfile) >>= \filetuples
                    -> (joinpath<$> dirs) >>= \subdirpaths
                    -> (subdirpaths >>~ foreachdir) >>= \recursed
                    -> return (concat [filetuples, concat recursed])
    in allfiles >>= \allfiletuples
    -> let foreachfiletuple (srcfilepath , modtime) =
            ( Util.atOr relpaths 0 srcfilepath,
                FileInfo srcfilepath modtime )
            where
            relpaths = dirpaths>~foreachsrcdir ~|noNull
            foreachsrcdir reldirpath =
                if Util.startsWith srcfilepath reldirpath then
                    drop (1+reldirpath~:length) srcfilepath else ""
        in return$ allfiletuples>~foreachfiletuple



pathSepSlashToSystem = Util.substitute '/' System.FilePath.pathSeparator

pathSepSystemToSlash = Util.substitute '\\' '/'
    --  no sys-sep: supports win data on posix



readOrDefault _ _ "" _ _ =
    return NoFile

readOrDefault create ctxmain relpath relpath2 defaultcontent =
    System.Directory.doesFileExist filepath >>= ifexists where
        filepath = System.FilePath.combine (ctxmain~:dirPath) relpath
        ifexists True =
            System.Directory.getModificationTime filepath >>= \ modtime
            -> readFile filepath >>= (FileFull filepath modtime)~.return
        ifexists False
            | (noNull relpath2 && relpath2/=relpath) =
                readOrDefault create ctxmain relpath2 "" defaultcontent
            | otherwise =
                let file = FileFull filepath (ctxmain~:nowTime) defaultcontent
                in if not create then return file else
                    writeTo filepath relpath (defaultcontent~:return)
                    >> return file



saneDirPath =
    Util.trim ~. pathSepSlashToSystem ~.
        (Util.trim' $'.':System.FilePath.pathSeparators) ~. Util.trim



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



_fileoutputbeginmsg = ("\t>>\t"++).(++"  [ ")

writeTo filepath showpath loadcontent =
    let _fileoutputdonemsg = "OK ]"
    in System.Directory.createDirectoryIfMissing True (System.FilePath.takeDirectory filepath)
    >> if null showpath then return () else
        putStr (_fileoutputbeginmsg showpath)
    >> System.IO.hFlush System.IO.stdout
    >> loadcontent >>= writeFile filepath
    >> putStrLn _fileoutputdonemsg
    >> System.IO.hFlush System.IO.stdout
