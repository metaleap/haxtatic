{-# OPTIONS_GHC -Wall #-}

module Build where

import qualified Files
import qualified Proj
import qualified ProjCfg
import qualified ProjDefaults
import Util ( (~.) , (~>) , (>~) , (#) )

import qualified Control.Monad
import System.Directory
import System.FilePath ( (</>) )


data Plan = Plan {
    inFilePosts :: [FileInfo],
    outFileGens :: [FileInfo],
    outFileCopies :: [FileInfo],
    numSkippedStatic :: Int,
    numSkippedPages :: Int
} deriving (Show)

data FileInfo = FileInfo {
    srcFile :: Files.File,
    relPath :: String,
    outPaths :: (String , String)
} deriving (Show)



plan ctxproj =
    let cfg = ctxproj~>Proj.setup~>Proj.cfg
        cfgprocstatic = cfg~>ProjCfg.processStatic
        cfgprocpages = cfg~>ProjCfg.processPages
        listallfiles = Files.listAllFiles $ctxproj~>Proj.dirPath
        modtimeproj = ctxproj~>Proj.coreFiles~>ProjDefaults.projectDefault~>Files.modTime
        modtimetmplmain = ctxproj~>Proj.coreFiles~>ProjDefaults.htmlTemplateMain~>Files.modTime
        modtimetmplblok = ctxproj~>Proj.coreFiles~>ProjDefaults.htmlTemplateBlok~>Files.modTime
    in listallfiles (cfgprocpages~>ProjCfg.dirs) (max modtimetmplmain) >>= \allpagesfiles
    -> listallfiles (cfgprocstatic~>ProjCfg.dirs) id >>= \allstaticfiles
    -> listallfiles (cfg~>ProjCfg.processPosts~>ProjCfg.dirs) (max modtimeproj) >>= \allpostsfiles
    -> let
        tofileinfo = fileInfo ctxproj
        allstatics = allstaticfiles >~ tofileinfo
        allpages = allpagesfiles >~ tofileinfo
        allposts = allpostsfiles >~ tofileinfo
    in filterFiles allstatics cfgprocstatic >>= \newstatics
    -> filterFiles allpages cfgprocpages >>= \newpages
    -> return Plan {
                inFilePosts = allposts,
                outFileGens = newpages,
                outFileCopies = newstatics,
                numSkippedStatic = allstatics~>length - newstatics~>length,
                numSkippedPages = allpages~>length - newpages~>length
            }



fileInfo ctxproj (relpath,file) =
    let outdirpaths = ctxproj~>Proj.outDirPaths
        fileinfo = FileInfo {
                        srcFile = file,
                        relPath = relpath,
                        outPaths = ( (fst outdirpaths)</>relpath , (snd outdirpaths)</>relpath )
                    }
    in fileinfo


filterFiles fileinfos cfgproc =
    let
        skipall = ["*"]== cfgproc~>ProjCfg.skip
        forceall = ["*"]== cfgproc~>ProjCfg.force
        shouldbuildfile fileinfo =
            let outfilepath = fileinfo~>(outPaths~.fst)
                matchesany = Files.simpleFileNameMatchAny $fileinfo~>relPath
                skipthis = (not skipall) && (matchesany $cfgproc~>ProjCfg.skip)
                forcethis = (not forceall) && (matchesany $cfgproc~>ProjCfg.force)
            in if (forceall && not skipthis) || forcethis then return True else
                if (skipall && not forcethis) || skipthis then return False else
                    outfilepath ~> System.Directory.doesFileExist >>= \ isfile
                    -> if not isfile then return True else
                        System.Directory.getModificationTime outfilepath >>= \ outfilemodtime
                        -> return ((fileinfo~>srcFile~>Files.modTime) > outfilemodtime)
    in Control.Monad.filterM shouldbuildfile fileinfos
