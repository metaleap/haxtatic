module ProjC where

import Base
import qualified Lst
import qualified Str

import qualified Defaults
import qualified Files
import qualified Tmpl
import qualified Util

import qualified Data.Map.Strict
import qualified Data.Time
import qualified Data.Time.Clock
import qualified Data.Time.Format
import qualified System.FilePath


data Config
    = CfgFromProj {
        dirNameBuild :: String,
        dirNameDeploy :: String,
        dirNameCache :: String,
        domainName :: String,
        relPathPostAtoms :: String,
        relPathSiteMap :: String,
        htmlEquivExts :: [String],
        dtFormat :: String->String,
        processingOfFiles :: Processing,
        processingOfPages :: Processing,
        parsingFailEarly :: Bool,
        tmplTags :: [String]
    }


data Processing
    = Proc {
        skip :: [String],
        force :: [String],
        dirs :: [String]
    }
    deriving Read



dtStr2Utc cfgproj dtfname str =
    (Data.Time.Format.parseTimeM
        True Data.Time.defaultTimeLocale (dtFormat cfgproj dtfname) str)
            :: Maybe Data.Time.Clock.UTCTime

dtStr2UtcOr cfgproj dtfname str defval =
    defval -|= (dtStr2Utc cfgproj dtfname str)

dtUtc2Str cfgproj dtfname utctime =
    let dtformat = (dtfname == "_hax_dtformat_iso8601")
                    |? Data.Time.Format.iso8601DateFormat (Just "%H:%M:%S")
                    |! dtFormat cfgproj dtfname
    in Data.Time.Format.formatTime Data.Time.defaultTimeLocale dtformat utctime



dtPageDateParse cfgproj = dtStr2Utc cfgproj "_hax_dtformat_pagefilenames"

dtPageDateFormat cfgproj = dtUtc2Str cfgproj "_hax_dtformat_pagefilenames"


parseProjChunks chunkssplits =
    (cfg,cfgmisc)
    where
    cfg = CfgFromProj { dirNameBuild = dirbuild, dirNameDeploy = dirdeploy, dirNameCache = dircache, domainName = domainname,
                        relPathPostAtoms = relpathpostatoms, relPathSiteMap = relpathsitemap,
                        htmlEquivExts = htmlequivexts, dtFormat = dtformat,
                        processingOfFiles = procstatic, processingOfPages = procpages, parsingFailEarly = parsingfailearly,
                        tmplTags = proctags }
    dtformat name = Data.Map.Strict.findWithDefault
                    Defaults.dateTimeFormat ("dtformat:"++name) cfgdtformats
    dirbuild = dirnameonly$ Data.Map.Strict.findWithDefault
                    Defaults.dir_Out "_hax_dir_build" cfgmisc
    dirdeploy = dirnameonly$ Data.Map.Strict.findWithDefault
                    Defaults.dir_Deploy "_hax_dir_deploy" cfgmisc
    dircache = dirnameonly$ Data.Map.Strict.findWithDefault
                    "_cache_tmp" "_hax_dir_cache" cfgmisc
    domainname = Files.sanitizeUriRelPathForJoin$ Data.Map.Strict.findWithDefault
                    "" "_hax_domainname" cfgmisc
    parsingfailearly = "abort" == (Data.Map.Strict.findWithDefault "" "_hax_onparseerror" cfgmisc)
    relpathsitemap = Files.sanitizeRelPath$ Data.Map.Strict.findWithDefault
                    "sitemap.xml" "_hax_relpath_sitemap" cfgmisc
    relpathpostatoms = Files.sanitizeRelPath$ Data.Map.Strict.findWithDefault
                    Defaults.dir_PostAtoms "_hax_relpath_postatoms" cfgmisc
    htmlequivexts = Util.unique ("":".html":".htm":hexts) where
        hexts = hstr ~> (Lst.splitOn ',') >~ (('.':).(Str.trimSpaceOr ['.']))
        hstr = Data.Map.Strict.findWithDefault "" "_hax_htmlequivexts" cfgmisc
    procstatic = procfind Defaults.dir_Static
    procpages = procfind Defaults.dir_Pages
    procfind name =
        let procstr = Data.Map.Strict.findWithDefault "" ("process:"++name) cfgprocs
            perr = procdef$ raiseParseErr "*.haxproj" ("|C|process:"++name++":") procstr
        in procsane name (Str.tryParseNonNull (procdef name) perr (("Proc {"++).(++"}")) procstr)

    procdef dirname =
        Proc { dirs = [dirname], skip = [], force = [] }
    procsane defname proc =
        Proc {
            dirs = (proc-:dirs >/~ dirnameonly) <?> [defname],
            skip = saneneither |? [] |! saneskip,
            force = saneneither |? [] |! saneforce
        } where
            saneneither = saneskip==saneforce
            saneskip = sanitize skip ; saneforce = sanitize force
            sanitize fvals = let them = proc~>fvals >/~ Str.trim
                                in (elem "*" them) |? ["*"] |! them
    proctags = (has ptags) |? ptags |! Tmpl.tags_All ~|(/=Tmpl.tag_C) where
        ptags = (pstr~>(Lst.splitOn ',') >/~ Str.trim) ~> Util.unique >~ (('{':).(++"|"))
        pstr = Str.trim$ Data.Map.Strict.findWithDefault "" ("process:tags") cfgprocs
    dirnameonly = System.FilePath.takeFileName ~. Str.trim
    cfgmisc = cfglines2hashmap ""
    cfgdtformats = cfglines2hashmap "dtformat"
    cfgprocs = cfglines2hashmap "process"
    cfglines2hashmap goalprefix =
        Data.Map.Strict.fromList$
            chunkssplits ~> Util.keepNoNilFsts foreachchunk where
                foreachchunk (prefix':next:rest)
                    | null goalprefix
                    = ( prefix , foreachvalue$ (next:rest) )
                    | prefix==goalprefix
                    = ( prefix ++ ":" ++ next~>Str.trim , foreachvalue$ rest )
                    where prefix = Str.trim prefix'
                foreachchunk _ = ( "" , "" )
                foreachvalue = (Lst.join ':') ~.Str.trim



raiseParseErr filehint directive parsestr =
    err $"!!=>\n\n\t"++filehint++" --- failed to parse..\n\t`"++directive++"`\n\t..due to some SYNTAX mistake somewhere in:\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"++parsestr++"\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\nLOOK OUT for: typos, missing-or-superfluous\n\tcommas/quotation/parens/brackets/braces\n\t\tor in the docs: /basics/syntax.html\n\n\n"



tagHandler cfgmisc key =
    Data.Map.Strict.lookup key cfgmisc
