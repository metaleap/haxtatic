{-# OPTIONS_GHC -Wall #-}

module Build where

import qualified Files
import qualified Proj
import qualified ProjCfg
import Util ( (~.) , (~>) , (>~) , (#) )

import qualified Control.Monad
import System.Directory
import System.FilePath ( (</>) )


data Plan = Plan {
    inFilePosts :: [FileInfo],
    outFileGens :: [FileInfo],
    outFileCopies :: [FileInfo]
} deriving (Show)

data FileInfo = FileInfo {
    srcFile :: Files.File,
    outPaths :: (String , String)
} deriving (Show)



plan ctxproj =
    let tofileinfo = fileInfo ctxproj
        cfg = ctxproj~>Proj.setup~>Proj.cfg
        listallfiles = Files.listAllFiles $ctxproj~>Proj.dirPath
    in listallfiles (cfg~>ProjCfg.processPages~>ProjCfg.dirs) >>= \allpagesfiles
    -> listallfiles (cfg~>ProjCfg.processStatic~>ProjCfg.dirs) >>= \allstaticfiles
    -> listallfiles (cfg~>ProjCfg.processPosts~>ProjCfg.dirs) >>= \allpostsfiles
    -> let
        allstatics = allstaticfiles >~ tofileinfo
        allpages = allpagesfiles >~ tofileinfo
        allposts = allpostsfiles >~ tofileinfo
    in filterFiles allstatics >>= \newstatics
    -> filterFiles allpages >>= \newpages
    -> return Plan { inFilePosts = allposts , outFileGens = newpages , outFileCopies = newstatics }



fileInfo ctxproj (relpath,file) =
    let outdirpaths = ctxproj~>Proj.outDirPaths
        fileinfo = FileInfo {
                        srcFile = file,
                        outPaths = ( (fst outdirpaths)</>relpath , (snd outdirpaths)</>relpath )
                    }
    in fileinfo


filterFiles fileinfos =
    Control.Monad.filterM shouldbuildfile fileinfos where
    shouldbuildfile fileinfo =
        fileinfo~>(outPaths~.fst) ~> System.Directory.doesFileExist >>= \ isfile
        -> if not isfile
            then return True
            else return False
