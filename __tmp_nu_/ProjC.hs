{-# OPTIONS_GHC -Wall #-}

module ProjC where

import qualified Defaults
import qualified Files
import qualified Util
import Util ( (~:) , (>~) , (~|) , (~.) , noNull )

import qualified Data.Map.Strict
import qualified Data.Maybe
import qualified System.FilePath
import qualified Text.Read


data Config
    = CfgFromProj {
        dirNameBuild :: String,
        dirNameDeploy :: String,
        relPathPostAtoms :: String,
        dtFormat :: String->String,
        processStatic :: Processing,
        processPages :: Processing,
        processPosts :: Processing
    }


data Processing
    = ProcFromProj {
        skip :: [String],
        force :: [String],
        dirs :: [String]
    }
    deriving (Read)



parseProjLines linessplits =
    CfgFromProj {   dirNameBuild = dirbuild, dirNameDeploy = dirdeploy,
                    relPathPostAtoms = relpathpostatoms, dtFormat = dtformat,
                    processStatic = procstatic, processPages = procpages, processPosts = procposts }
    where
        dtformat name = Data.Map.Strict.findWithDefault
                        Defaults.dateTimeFormat ("dtformat:"++name) cfgdtformats
        dirbuild = dirnameonly$ Data.Map.Strict.findWithDefault
                        Defaults.dir_Out "dir_build" cfgmisc
        dirdeploy = dirnameonly$ Data.Map.Strict.findWithDefault
                        Defaults.dir_Deploy "dir_deploy" cfgmisc
        relpathpostatoms = Files.saneDirPath$ Data.Map.Strict.findWithDefault
                        Defaults.dir_PostAtoms "posts_atomrelpath" cfgmisc
        procstatic = procfind Defaults.dir_Static
        procpages = procfind Defaults.dir_Pages
        procposts = procfind Defaults.dir_Posts
        procfind name = procsane name (Data.Maybe.fromMaybe (procdef name) maybeParsed) where
                            maybeParsed = (Text.Read.readMaybe procstr) :: Maybe Processing
                            procstr = Data.Map.Strict.findWithDefault "" ("process:"++name) cfgprocs
        procdef dirname = ProcFromProj { dirs = [dirname], skip = [], force = [] }
        procsane defname proc = ProcFromProj {
                dirs = Util.ifNull (proc~:dirs >~dirnameonly ~|noNull) [defname],
                skip = if saneneither then [] else saneskip,
                force = if saneneither then [] else saneforce
            } where
                saneneither = saneskip==saneforce
                saneskip = sanitize skip ; saneforce = sanitize force
                sanitize fvals = let tmp = proc~:fvals >~Util.trim ~|noNull in
                    if elem "*" tmp then ["*"] else tmp
        dirnameonly = System.FilePath.takeFileName
        cfgmisc = cfglines2hashmap "" id
        cfgdtformats = cfglines2hashmap "dtformat" id
        cfgprocs = cfglines2hashmap "process" $("ProcFromProj {"++).(++"}")
        cfglines2hashmap goalprefix onvalue = Data.Map.Strict.fromList$
            linessplits>~foreachline ~|noNull.fst where
                foreachline ("C":"":prefix:next:rest)
                    |(null goalprefix) = ( prefix , foreachvalue$ (next:rest) )
                    |(prefix==goalprefix) = ( prefix++":"++next , foreachvalue$ rest )
                foreachline _ = ( "" , "" )
                foreachvalue = (Util.join ":") ~. Util.trim ~. onvalue