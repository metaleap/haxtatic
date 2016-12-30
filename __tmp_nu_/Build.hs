{-# OPTIONS_GHC -Wall #-}

module Build where

import qualified Bloks
import qualified Files
import qualified Proj
import qualified ProjCfg
import qualified ProjDefaults
import Util ( (~:) , (>~) , (>>~) , (>>|) )

import qualified System.Directory
import qualified System.FilePath
import System.FilePath ( (</>) )


data Plan = Plan {
    outFilesAtom :: [FileInfo],
    outFilesPage :: [FileInfo],
    outFilesStatic :: [FileInfo],
    numOutFilesTotal :: Int,
    numSkippedStatic :: Int,
    numSkippedPages :: Int,
    numSkippedAtoms :: Int
} deriving (Show)

data FileInfo = FileInfo {
    relPath :: FilePath,
    outPathBuild :: FilePath,
    outPathDeploy :: FilePath,
    srcFile :: Files.File
} deriving (Show)



copyAllOutputsToDeploy buildplan =
    let perfile file =
            let srcfilepath = file~:outPathBuild
            in System.Directory.doesFileExist srcfilepath >>= \isfile
            -> if isfile
                then Files.copyTo srcfilepath [file~:outPathDeploy]
                else putStrLn ("\t!>\tWeirdly missing: "++srcfilepath)
    in (buildplan~:outFilesStatic) >>~ perfile
    >> (buildplan~:outFilesPage) >>~ perfile
    >> (buildplan~:outFilesAtom) >>~ perfile
    >> return ()



copyStaticFiles buildplan =
    (buildplan~:outFilesStatic) >>~ perfile where
        perfile file =
            Files.copyTo (file~:srcFile~:Files.path) [file~:outPathBuild]



_createIndexHtmlIfNoContentPages ctxproj buildplan =
    return buildplan



plan ctxproj =
    let projsetup = ctxproj~:Proj.setup
        cfg = projsetup~:Proj.cfg
        cfgprocstatic = cfg~:ProjCfg.processStatic
        cfgprocpages = cfg~:ProjCfg.processPages
        cfgprocposts = cfg~:ProjCfg.processPosts
        listallfiles = Files.listAllFiles $ctxproj~:Proj.dirPath
        modtimeproj = ctxproj~:Proj.coreFiles~:ProjDefaults.projectDefault~:Files.modTime
        modtimetmplmain = ctxproj~:Proj.coreFiles~:ProjDefaults.htmlTemplateMain~:Files.modTime
        modtimetmplblok = ctxproj~:Proj.coreFiles~:ProjDefaults.htmlTemplateBlok~:Files.modTime
    in listallfiles (cfgprocpages~:ProjCfg.dirs) (max modtimetmplmain) >>= \allpagesfiles
    -> listallfiles (cfgprocstatic~:ProjCfg.dirs) id >>= \allstaticfiles
    -> listallfiles (cfgprocposts~:ProjCfg.dirs) (max modtimeproj) >>= \allpostsfiles
    -> let
        allstatics = allstaticfiles >~ (_fileInfo ctxproj "")
        allpages = (allpagesfiles++dynpages) >~ (_fileInfo ctxproj "")
        allatoms = (allpostsfiles++dynatoms) >~ (_fileInfo ctxproj ".atom")
        (dynpages , dynatoms) = Bloks.buildPlan (modtimeproj,modtimetmplblok) allpagesfiles $projsetup~:Proj.bloks
    in _filterOutFiles allstatics cfgprocstatic >>= \outstatics
    -> _filterOutFiles allpages cfgprocpages >>= \outpages
    -> _filterOutFiles allatoms cfgprocposts >>= \outatoms
    -> let buildplan = Plan {
                outFilesAtom = outatoms,
                outFilesPage = outpages,
                outFilesStatic = outstatics,
                numOutFilesTotal = outstatics~:length + outpages~:length + outatoms~:length,
                numSkippedStatic = allstatics~:length - outstatics~:length,
                numSkippedPages = allpages~:length - outpages~:length,
                numSkippedAtoms = allatoms~:length - outatoms~:length
            }
    in _createIndexHtmlIfNoContentPages ctxproj buildplan



_fileInfo ctxproj addext (relpath,file) =
    let relpathext = Files.ensureFileExt relpath addext
        fileinfo = FileInfo {
                        relPath = relpathext,
                        outPathBuild = ctxproj~:Proj.dirPathBuild </> relpathext,
                        outPathDeploy = let dd = ctxproj~:Proj.dirPathDeploy
                                        in if null dd then "" else dd </> relpathext,
                        srcFile = file
                    }
    in fileinfo


_filterOutFiles fileinfos cfgproc =
    let
        skipall = ["*"]== cfgproc~:ProjCfg.skip
        forceall = ["*"]== cfgproc~:ProjCfg.force
        shouldbuildfile fileinfo =
            let outfilepath = fileinfo~:outPathBuild
                matchesany = Files.simpleFileNameMatchAny $fileinfo~:relPath
                skipthis = (not skipall) && (matchesany $cfgproc~:ProjCfg.skip)
                forcethis = (not forceall) && (matchesany $cfgproc~:ProjCfg.force)
            in if (forceall && not skipthis) || forcethis then return True else
                if (skipall && not forcethis) || skipthis then return False else
                    outfilepath ~: System.Directory.doesFileExist >>= \ isfile
                    -> if not isfile then return True else
                        System.Directory.getModificationTime outfilepath >>= \ outfilemodtime
                        -> return ((fileinfo~:srcFile~:Files.modTime) > outfilemodtime)
    in fileinfos >>| shouldbuildfile
