{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Files where

import Hax.Base

import qualified Util

import qualified Data.List
import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.FilePath
import System.FilePath ( (</>) )

import qualified System.IO



--  general project input file
data File
    = NoFile
    | FileInfo {
        path :: FilePath,
        modTime :: Data.Time.Clock.UTCTime
    }
    | FileFull {
        path :: FilePath,
        modTime :: Data.Time.Clock.UTCTime,
        content :: String
    }
    deriving Eq


data Ctx
    = AppContext {
        curDir :: FilePath,
        dirPath :: FilePath,
        setupName :: String,
        nowTime :: Data.Time.Clock.UTCTime,
        randSeed :: [Int]
    }



_isfsnameok ('.':_) = False
_isfsnameok _ = True



copyTo _ [] = pure ()
copyTo srcfilepath ("":dstmore) = copyTo srcfilepath dstmore
copyTo srcfilepath (dstpath:dstmore) =
    System.Directory.createDirectoryIfMissing True (System.FilePath.takeDirectory dstpath)
    *> System.Directory.copyFile srcfilepath dstpath
    *> copyTo srcfilepath dstmore



customDateFromFileName dateparser (filepath , srcfile) =
    let
        filedir = System.FilePath.takeDirectory filepath
        filename = System.FilePath.takeFileName $srcfile-:path
        (datepart , fnrest) = System.FilePath.splitExtensions filename
    in case (dateparser datepart) of
        Just customdate->
            ( (sanitizeRelPath filedir) </> (Util.trimStart' ['.'] fnrest) , customdate )
        Nothing->
            ( filepath , srcfile-:modTime )



ensureFileExt _ "" filepath = filepath
ensureFileExt _ _ "" = ""
ensureFileExt ignorecase ext filepath =
    let curext = System.FilePath.takeExtension filepath
        cmp = if ignorecase then Util.toLower else id
    in if (cmp curext)==(cmp ext)
        then filepath else filepath++ext



filesInDir dir =
    System.Directory.doesDirectoryExist dir >>= direxists where
        direxists False =
            pure []
        direxists True =
            System.Directory.listDirectory dir >>=
            (~| _isfsnameok) ~. (>>| filesonly) where
                filesonly = (dir</>) ~. System.Directory.doesFileExist



hasAnyFileExt exts filename =
    any ((==) (System.FilePath.takeExtension filename)) exts



hasPathBlokIndexPrefix (':':'B':'|':'/':_) = True
hasPathBlokIndexPrefix _ = False

prependPathBlokIndexPrefix fauxpath = ':':'B':'|':'/':fauxpath


listAllFiles rootdirpath reldirs modtimer =
    let allfiles = (dirpaths >>~ foreachdir) >~ concat
        dirpaths = reldirs >~ (System.FilePath.combine rootdirpath)
        foreachfile :: FilePath -> IO (FilePath , Data.Time.Clock.UTCTime)
        foreachfile filepath =
            System.Directory.getModificationTime filepath >>= \modtime
            -> pure (filepath , modtimer modtime)
        foreachdir :: FilePath -> IO [(FilePath , Data.Time.Clock.UTCTime)]
        foreachdir dirpath =
            System.Directory.doesDirectoryExist dirpath >>= direxists where
                direxists False =
                    pure []
                direxists True =
                    System.Directory.listDirectory dirpath >>= \names
                    -> let  oknames = names ~| _isfsnameok
                            files = oknames >>| fstest System.Directory.doesFileExist
                            dirs = oknames >>| fstest System.Directory.doesDirectoryExist
                            fstest = ((dirpath</>) ~.)
                            joinpath n = n >>= pure.(dirpath</>)
                    in (joinpath<$> files) >>= \filepaths
                    -> (filepaths >>~ foreachfile) >>= \filetuples
                    -> (joinpath<$> dirs) >>= \subdirpaths
                    -> (subdirpaths >>~ foreachdir) >>= \recursed
                    -> pure (concat [filetuples, concat recursed])
    in allfiles >>= \allfiletuples
    -> let
        foreachfiletuple (srcfilepath , modtime) =
            ( srcfilepath -|= relpaths@?0,
                FileInfo srcfilepath modtime )
            where
            relpaths = dirpaths>=~foreachsrcdir
            foreachsrcdir reldirpath =
                if Util.startsWith srcfilepath reldirpath
                then Just$ drop (1 + reldirpath~>length) srcfilepath else Nothing
        newest (_,modtime1) (_,modtime2) = compare modtime2 modtime1
    in pure$ (Data.List.sortBy newest allfiletuples)>~foreachfiletuple



pathSepSlashToSystem = Util.substitute '/' System.FilePath.pathSeparator

pathSepSystemToSlash = Util.substitute '\\' '/' -- no sys-sep here: thus covers win data on posix



readOrDefault _ _ "" _ _ =
    pure NoFile

readOrDefault create ctxmain relpath relpath2 defaultcontent =
    System.Directory.doesFileExist filepath >>= fileexists
    where
    filepath = System.FilePath.combine (ctxmain-:dirPath) relpath
    fileexists True =
        System.Directory.getModificationTime filepath >>= \modtime
        -> readFile filepath >>= (FileFull filepath modtime)~.pure
    fileexists False =
        if has relpath2 && relpath2/=relpath
        then readOrDefault create ctxmain relpath2 "" defaultcontent else
        let file = FileFull filepath filetime defaultcontent
            filetime = if create || has defaultcontent then (ctxmain-:nowTime) else Util.dateTime0
        in if not create then pure file else
            writeTo filepath relpath (pure (defaultcontent , ()))
            *> pure file



sanitizeRelPath =
    (Util.trimSpaceOr ('.':System.FilePath.pathSeparators)) ~. pathSepSlashToSystem

sanitizeUriRelPathForJoin =
    (Util.trimSpaceOr System.FilePath.pathSeparators) ~. pathSepSystemToSlash



simpleFileNameMatch =
    simpleFilePathMatch . System.FilePath.takeFileName

simpleFileNameMatchAny =
    simpleFilePathMatchAny . System.FilePath.takeFileName

simpleFilePathMatch :: FilePath -> String -> Bool
simpleFilePathMatch _ "*" = True
simpleFilePathMatch relpath dumbpattern =
    let testcontains = patternstarts && patternends
        teststarts = (not testcontains) && patternends
        testends = (not testcontains) && patternstarts
        patternstarts = Util.begins '*' dumbpattern
        patternends = Util.endsWith dumbpattern "*"
    in (testcontains && Util.contains relpath (Util.crop 1 1 dumbpattern))
    || (teststarts && Util.startsWith relpath (Util.crop 0 1 dumbpattern))
    || (testends && Util.endsWith relpath (Util.crop 1 0 dumbpattern))

simpleFilePathMatchAny :: FilePath -> [String] -> Bool
simpleFilePathMatchAny relpath dumbpatterns =
    any (simpleFilePathMatch relpath) dumbpatterns



_fileoutputbeginmsg = ("\t>>\t" ++).(++ _fileoutputmidmsg)
_fileoutputmidmsg = " [ "
_fileoutputdonemsg = "OK ]"



writeTo "" _ loadcontent =
    loadcontent >>= \(_ , finalresult)
    -> pure finalresult
writeTo filepath showpath loadcontent =
    System.Directory.createDirectoryIfMissing True (System.FilePath.takeDirectory filepath)
    *> (has showpath |? putStr (_fileoutputbeginmsg showpath) |! pure ())
    *> System.IO.hFlush System.IO.stdout
    *> loadcontent >>= \(loadedcontent , finalresult)
    -> writeFile filepath loadedcontent
    *> (has showpath |? putStrLn _fileoutputdonemsg |! pure ())
    *> System.IO.hFlush System.IO.stdout
    *> pure finalresult
