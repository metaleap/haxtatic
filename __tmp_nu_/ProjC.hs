{-# OPTIONS_GHC -Wall #-}

module ProjC where

import qualified Defaults
import qualified Files
import qualified Tmpl
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
        processPosts :: Processing,
        tmplTags :: [String]
    }


data Processing
    = ProcFromProj {
        skip :: [String],
        force :: [String],
        dirs :: [String]
    }
    deriving (Read)



parseProjLines linessplits =
    (cfg,cfgmisc)
    where
    cfg = CfgFromProj { dirNameBuild = dirbuild, dirNameDeploy = dirdeploy,
                        relPathPostAtoms = relpathpostatoms, dtFormat = dtformat,
                        processStatic = procstatic, processPages = procpages, processPosts = procposts,
                        tmplTags = proctags }
    dtformat name = Data.Map.Strict.findWithDefault
                    Defaults.dateTimeFormat ("dtformat:"++name) cfgdtformats
    dirbuild = dirnameonly$ Data.Map.Strict.findWithDefault
                    Defaults.dir_Out "_hax_dir_build" cfgmisc
    dirdeploy = dirnameonly$ Data.Map.Strict.findWithDefault
                    Defaults.dir_Deploy "_hax_dir_deploy" cfgmisc
    relpathpostatoms = Files.saneDirPath$ Data.Map.Strict.findWithDefault
                    Defaults.dir_PostAtoms "_hax_posts_atomrelpath" cfgmisc
    procstatic = procfind Defaults.dir_Static
    procpages = procfind Defaults.dir_Pages
    procposts = procfind Defaults.dir_Posts
    procfind name =
        procsane name (Data.Maybe.fromMaybe (procdef name) maybeParsed) where
            maybeParsed = if null procstr then Nothing
                            else (Text.Read.readMaybe procstr) :: Maybe Processing
            procstr = if null procval then procval else "ProcFromProj {"++procval++"}"
            procval = Data.Map.Strict.findWithDefault "" ("process:"++name) cfgprocs
    procdef dirname =
        ProcFromProj { dirs = [dirname], skip = [], force = [] }
    procsane defname proc =
        ProcFromProj {
            dirs = Util.ifNull (proc~:dirs >~dirnameonly ~|noNull) [defname],
            skip = if saneneither then [] else saneskip,
            force = if saneneither then [] else saneforce
        } where
            saneneither = saneskip==saneforce
            saneskip = sanitize skip ; saneforce = sanitize force
            sanitize fvals = let tmp = proc~:fvals >~Util.trim ~|noNull in
                if elem "*" tmp then ["*"] else tmp
    proctags = if noNull ptags then ptags else Tmpl.tags_All where
        ptags = (pstr~:(Util.splitBy ',') >~ Util.trim ~|noNull) >~ ('{':).(++"{")
        pstr = Util.trim$ Data.Map.Strict.findWithDefault "" ("process:tags") cfgprocs
    dirnameonly = System.FilePath.takeFileName
    cfgmisc = cfglines2hashmap ""
    cfgdtformats = cfglines2hashmap "dtformat"
    cfgprocs = cfglines2hashmap "process"
    cfglines2hashmap goalprefix = -- onvalue =
        Data.Map.Strict.fromList$
            linessplits>~foreachline ~|noNull.fst where
                foreachline ("|C|":prefix':next:rest)
                    |(null goalprefix) = ( prefix , foreachvalue$ (next:rest) )
                    |(prefix==goalprefix) = ( prefix ++":"++ next~:Util.trim , foreachvalue$ rest )
                    where prefix = Util.trim prefix'
                foreachline _ = ( "" , "" )
                foreachvalue = (Util.join ":") ~.Util.trim -- ~. onvalue



tagResolver skip cfgmisc key =
    if skip then Nothing else
        Data.Map.Strict.lookup key cfgmisc
