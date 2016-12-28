{-# OPTIONS_GHC -Wall #-}

module Build where

import qualified Files
import qualified Proj
import qualified ProjCfg
import Util ( (~>) , (>~) , (#) )



data Plan = Plan {
    fileCopies :: [OutFile],
    fileGens :: [OutFile]
} deriving (Show)

data OutFile = OutFile {
    dstPath :: String,
    srcPath :: String
} deriving (Show)



plan ctxproj =
    let cfg = ctxproj~>Proj.setup~>Proj.cfg
    in
    (Files.listAllFiles (ctxproj~>Proj.dirPath) (cfg~>ProjCfg.processStatic~>ProjCfg.dirs)) >>= print
    --(Files.listAllFiles (ctxproj~>Proj.dirPath) (cfg~>ProjCfg.processStatic~>ProjCfg.dirs)) >>= print
    >> return Plan { fileCopies = statics , fileGens = pages } where
        statics = []
        pages = []
