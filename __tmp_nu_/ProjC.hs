{-# OPTIONS_GHC -Wall #-}
module ProjC where

import qualified Defaults
import qualified Files
import qualified Tmpl
import qualified Util
import Util ( (~:) , (>~) , (~|) , (~.) , (|?) , (|!) , noNull )

import qualified Data.Map.Strict
import qualified Data.Maybe
import qualified Data.Time
import qualified Data.Time.Clock
import qualified Data.Time.Format
import qualified System.FilePath


data Config
    = CfgFromProj {
        dirNameBuild :: String,
        dirNameDeploy :: String,
        domainName :: String,
        relPathPostAtoms :: String,
        relPathSiteMap :: String,
        htmlEquivExts :: [String],
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



dtStr2Utc cfgproj dtfname str =
    (Data.Time.Format.parseTimeM
        True Data.Time.defaultTimeLocale (dtFormat cfgproj dtfname) str)
            :: Maybe Data.Time.Clock.UTCTime

dtStr2UtcOr cfgproj dtfname str defval =
    case dtStr2Utc cfgproj dtfname str of
        Just parsed -> parsed
        Nothing -> defval

dtUtc2Str cfgproj dtfname utctime =
    let dtformat = (dtfname == "_hax_dtformat_iso8601")
                    |? Data.Time.Format.iso8601DateFormat (Just "%H:%M:%S")
                    |! dtFormat cfgproj dtfname
    in Data.Time.Format.formatTime Data.Time.defaultTimeLocale dtformat utctime



dtPageDateParse cfgproj = dtStr2Utc cfgproj "_hax_dtformat_pagefilenames"

dtPageDateFormat cfgproj = dtUtc2Str cfgproj "_hax_dtformat_pagefilenames"


parseProjLines linessplits =
    (cfg,cfgmisc)
    where
    cfg = CfgFromProj { dirNameBuild = dirbuild, dirNameDeploy = dirdeploy, domainName = domainname,
                        relPathPostAtoms = relpathpostatoms, relPathSiteMap = relpathsitemap,
                        htmlEquivExts = htmlequivexts, dtFormat = dtformat,
                        processStatic = procstatic, processPages = procpages, processPosts = procposts,
                        tmplTags = proctags }
    dtformat name = Data.Map.Strict.findWithDefault
                    Defaults.dateTimeFormat ("dtformat:"++name) cfgdtformats
    dirbuild = dirnameonly$ Data.Map.Strict.findWithDefault
                    Defaults.dir_Out "_hax_dir_build" cfgmisc
    dirdeploy = dirnameonly$ Data.Map.Strict.findWithDefault
                    Defaults.dir_Deploy "_hax_dir_deploy" cfgmisc
    domainname = dirnameonly$ Data.Map.Strict.findWithDefault
                    "" "_hax_domainname" cfgmisc
    relpathsitemap = Files.sanitizeRelPath$ Data.Map.Strict.findWithDefault
                    "sitemap.xml" "_hax_relpath_sitemap" cfgmisc
    relpathpostatoms = Files.sanitizeRelPath$ Data.Map.Strict.findWithDefault
                    Defaults.dir_PostAtoms "_hax_relpath_postatoms" cfgmisc
    htmlequivexts = Util.unique (htmldefexts ++ hexts) where
        htmldefexts = ["",".html",".htm"]
        hexts = hstr~:(Util.splitBy ',') >~ (('.':) . Util.trim . (Util.trim' ['.']) . Util.trim)
        hstr = Data.Map.Strict.findWithDefault "" "_hax_htmlequivexts" cfgmisc
    procstatic = procfind Defaults.dir_Static
    procpages = procfind Defaults.dir_Pages
    procposts = procfind Defaults.dir_Posts
    procfind name =
        procsane name (Util.tryParseOr (procdef name) procstr) where
            procstr = (null procval) |? procval |! "ProcFromProj "++procval
            procval = Data.Map.Strict.findWithDefault "" ("process:"++name) cfgprocs
    procdef dirname =
        ProcFromProj { dirs = [dirname], skip = [], force = [] }
    procsane defname proc =
        ProcFromProj {
            dirs = Util.ifNull (proc~:dirs >~dirnameonly ~|noNull) [defname],
            skip = Util.when saneneither [] saneskip,
            force = Util.when saneneither [] saneforce
        } where
            saneneither = saneskip==saneforce
            saneskip = sanitize skip ; saneforce = sanitize force
            sanitize fvals = let them = proc~:fvals >~Util.trim ~|noNull
                                in (elem "*" them) |? ["*"] |! them
    proctags = (noNull ptags) |? ptags |! Tmpl.tags_All where
        ptags = (pstr~:(Util.splitBy ',') >~ Util.trim ~|noNull) ~:Util.unique >~('{':).(++"|")
        pstr = Util.trim$ Data.Map.Strict.findWithDefault "" ("process:tags") cfgprocs
    dirnameonly = System.FilePath.takeFileName ~. Util.trim
    cfgmisc = cfglines2hashmap ""
    cfgdtformats = cfglines2hashmap "dtformat"
    cfgprocs = cfglines2hashmap "process"
    cfglines2hashmap goalprefix = -- onvalue =
        Data.Map.Strict.fromList$
            linessplits>~foreachline ~|noNull.fst where
                foreachline ("|C|":prefix':next:rest)
                    | null goalprefix
                    = ( prefix , foreachvalue$ (next:rest) )
                    | prefix==goalprefix
                    = ( prefix ++":"++ next~:Util.trim , foreachvalue$ rest )
                    where prefix = Util.trim prefix'
                foreachline _ = ( "" , "" )
                foreachvalue = (Util.join ":") ~.Util.trim -- ~. onvalue



tagResolver cfgmisc key =
    Data.Map.Strict.lookup key cfgmisc
