{-# OPTIONS_GHC -Wall #-}

module Build where

import qualified Bloks
import qualified Files
import qualified Proj
import qualified ProjCfg
import qualified ProjDefaults
import Util ( (~.) , (~>) , (>~) , (#) )

import qualified Control.Monad
import System.Directory
import System.FilePath ( (</>) )


data Plan = Plan {
    outFileAtoms :: [FileInfo],
    outFilePages :: [FileInfo],
    outFileStatic :: [FileInfo],
    numSkippedStatic :: Int,
    numSkippedPages :: Int,
    numSkippedAtoms :: Int
} deriving (Show)

data FileInfo = FileInfo {
    relPath :: FilePath,
    outPath :: FilePath,
    srcFile :: Files.File
} deriving (Show)



plan ctxproj =
    let projsetup = ctxproj~>Proj.setup
        cfg = projsetup~>Proj.cfg
        cfgprocstatic = cfg~>ProjCfg.processStatic
        cfgprocpages = cfg~>ProjCfg.processPages
        cfgprocposts = cfg~>ProjCfg.processPosts
        listallfiles = Files.listAllFiles $ctxproj~>Proj.dirPath
        modtimeproj = ctxproj~>Proj.coreFiles~>ProjDefaults.projectDefault~>Files.modTime
        modtimetmplmain = ctxproj~>Proj.coreFiles~>ProjDefaults.htmlTemplateMain~>Files.modTime
        modtimetmplblok = ctxproj~>Proj.coreFiles~>ProjDefaults.htmlTemplateBlok~>Files.modTime
    in listallfiles (cfgprocpages~>ProjCfg.dirs) (max modtimetmplmain) >>= \allpagesfiles
    -> listallfiles (cfgprocstatic~>ProjCfg.dirs) id >>= \allstaticfiles
    -> listallfiles (cfgprocposts~>ProjCfg.dirs) (max modtimeproj) >>= \allpostsfiles
    -> let
        tofileinfo = fileInfo ctxproj
        allstatics = allstaticfiles >~ (tofileinfo "")
        allpages = (allpagesfiles++dynpages) >~ (tofileinfo "")
        allatoms = (allpostsfiles++dynatoms) >~ (tofileinfo ".atom")
        (dynpages , dynatoms) = Bloks.buildPlan (modtimeproj,modtimetmplblok) allpagesfiles $projsetup~>Proj.bloks
    in filterFiles allstatics cfgprocstatic >>= \newstatics
    -> filterFiles allpages cfgprocpages >>= \newpages
    -> filterFiles allatoms cfgprocposts >>= \newatoms
    -> return Plan {
                outFileAtoms = allatoms,
                outFilePages = newpages,
                outFileStatic = newstatics,
                numSkippedStatic = allstatics~>length - newstatics~>length,
                numSkippedPages = allpages~>length - newpages~>length,
                numSkippedAtoms = allatoms~>length - newatoms~>length
            }



fileInfo ctxproj addext (relpath,file) =
    let outdirpaths = ctxproj~>Proj.outDirPaths
        relpathext = Files.ensureFileExt relpath addext
        fileinfo = FileInfo {
                        relPath = relpathext,
                        outPath = (fst outdirpaths)</>relpathext,
                        srcFile = file
                    }
    in fileinfo


filterFiles fileinfos cfgproc =
    let
        skipall = ["*"]== cfgproc~>ProjCfg.skip
        forceall = ["*"]== cfgproc~>ProjCfg.force
        shouldbuildfile fileinfo =
            let outfilepath = fileinfo~>outPath
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
