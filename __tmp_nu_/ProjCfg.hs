{-# OPTIONS_GHC -Wall #-}

module ProjCfg where

import qualified ProjDefaults
import qualified Util
import Util ( (~:) , (>~) , (~|) , noNull )

import qualified Data.Map.Strict
import qualified Data.Maybe
import qualified System.FilePath
import qualified Text.Read


data Cfg = Cfg {
    dirNameBuild :: String,
    dirNameDeploy :: String,
    processStatic :: Processing,
    processPages :: Processing,
    processPosts :: Processing
} deriving (Read, Show)

data Processing = Processing {
    skip :: [String],
    force :: [String],
    dirs :: [String]
} deriving (Read, Show)



parseDefs linessplits =
    Cfg {   dirNameBuild = dirbuild, dirNameDeploy = dirdeploy,
            processStatic = procstatic, processPages = procpages, processPosts = procposts }
    where
        dirbuild = dirnameonly$ Data.Map.Strict.findWithDefault
                        ProjDefaults.dir_Out "dir_build" cfgmisc
        dirdeploy = dirnameonly$ Data.Map.Strict.findWithDefault
                        "" "dir_deploy" cfgmisc
        procstatic = procfind ProjDefaults.dir_Static
        procpages = procfind ProjDefaults.dir_Pages
        procposts = procfind ProjDefaults.dir_Posts
        procfind name = procsane name $ Data.Maybe.fromMaybe (procdef name) $
                        Data.Map.Strict.findWithDefault Nothing ("process:"++name) cfgprocs
        procdef dirname = Processing { dirs = [dirname], skip = [], force = [] }
        procsane defname proc = Processing {
                dirs = Util.ifNull (proc~:dirs >~dirnameonly ~|noNull) [defname],
                skip = if saneneither then [] else saneskip,
                force = if saneneither then [] else saneforce
            } where
                saneneither = saneskip==saneforce
                saneskip = sanitize skip ; saneforce = sanitize force
                sanitize fvals = let tmp = proc~:fvals >~Util.trim ~|noNull in
                    if elem "*" tmp then ["*"] else tmp
        dirnameonly = System.FilePath.takeFileName . Util.trim
        cfgprocs = Data.Map.Strict.fromList$
            linessplits>~perprocsplit ~|noNull.fst where
            perprocsplit ("C":"":"process":name:procstr) =
                ( "process:"++name ,
                    Text.Read.readMaybe$ "Processing {"++(Util.join ":" procstr)++"}"
                        :: Maybe Processing )
            perprocsplit _ =
                ( "" , Nothing )
        cfgmisc = Data.Map.Strict.fromList$
            linessplits>~permiscsplit ~|noNull.fst where
            permiscsplit ("C":"":cfgname:cfgvals) = ( cfgname , Util.join ":" cfgvals )
            permiscsplit _ = ( "" , "" )
