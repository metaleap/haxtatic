{-# OPTIONS_GHC -Wall #-}

module Proj where

import qualified Bloks
import qualified Defaults
import qualified Files
import qualified ProjC
import qualified ProjT
import qualified Util
import Util ( (~:) , (>~) )

import qualified Tmpl

import qualified Data.Map.Strict
import qualified Data.Time
import qualified Data.Time.Clock
import qualified Data.Time.Format
import System.FilePath ( (</>) )



--  project context
data Ctx
    = ProjContext {
        projName :: String,
        setupName :: String,
        dirPath :: FilePath,
        dirPathBuild :: FilePath,
        dirPathDeploy :: FilePath,
        setup :: Setup,
        coreFiles :: Defaults.Files
    }


data Setup
    = SetupFromProj {
        --  srcRaw :: [String],
        --  srcPre :: [String],
        bloks :: Data.Map.Strict.Map String Bloks.Blok,
        cfg :: ProjC.Config,
        tmpl :: Tmpl.Ctx
    }



dtStr2Utc projcfg dtfname str =
    (Data.Time.Format.parseTimeM
        True Data.Time.defaultTimeLocale (ProjC.dtFormat projcfg dtfname) str)
            :: Maybe Data.Time.Clock.UTCTime

dtStr2UtcOr projcfg dtfname str defval =
    case dtStr2Utc projcfg dtfname str of
        Just parsed -> parsed
        Nothing -> defval

dtUtc2Str projcfg dtfname utctime =
    Data.Time.Format.formatTime Data.Time.defaultTimeLocale (ProjC.dtFormat projcfg dtfname) utctime



loadCtx ctxmain projname defaultfiles =
    let loadedsetup = _loadSetup ctxproj
        dirpath = ctxmain~:Files.dirPath
        dirpathjoin = (dirpath </>)
        setupname = ctxmain~:Files.setupName
        ctxproj = ProjContext {
            projName = projname,
            setupName = setupname,
            dirPath = dirpath,
            dirPathBuild = dirpathjoin $setupname++"-"++loadedsetup~:cfg~:ProjC.dirNameBuild,
            dirPathDeploy = let dd = loadedsetup~:cfg~:ProjC.dirNameDeploy
                            in if null dd then "" else dirpathjoin $setupname++"-"++dd,
            setup = loadedsetup,
            coreFiles = defaultfiles
        }
    in return ctxproj



_loadSetup ctxproj =
    let setuppost = SetupFromProj { -- srcRaw = srclinespost, srcPre = srclinesprep,
                                    bloks = blokspost,
                                    cfg = cfgpost,
                                    tmpl = Tmpl.Processing {
                                            Tmpl.bTags =  Bloks.tagResolver Tmpl.Postpone blokspost,
                                            Tmpl.tTags = ProjT.tagResolver Tmpl.Failed ttagspost
                                        }
                                    }
    in setuppost
    where
        setupprep = SetupFromProj { -- srcRaw = [], srcPre = [],
                                    bloks = bloksprep,
                                    cfg = cfgprep,
                                    tmpl = Tmpl.Processing {
                                            Tmpl.bTags =  Bloks.tagResolver Tmpl.Postpone bloksprep,
                                            Tmpl.tTags = ProjT.tagResolver Tmpl.Postpone ttagsprep
                                        }
                                    }
        bloksprep = Bloks.parseProjLines preplinessplits
        blokspost = Bloks.parseProjLines postlinessplits
        cfgprep = ProjC.parseProjLines preplinessplits
        cfgpost = ProjC.parseProjLines postlinessplits
        ttagsprep = ProjT.parseProjLines preplinessplits False
        ttagspost = ProjT.parseProjLines postlinessplits True

        preplinessplits = srclinesprep>~ _splitc
        postlinessplits = srclinespost>~ _splitc
        _splitc = Util.splitBy ':'
        srclinesprep = ProjT.srcLinesExpandMl$ _rawsrc ctxproj
        srclinespost = lines$ Tmpl.processSrcFully (setupprep~:tmpl) "" (srclinesprep~:unlines)



_rawsrc ctxproj =
    --  join primary project file with additionally-specified 'overwrites' ones:
    (ctxproj~:coreFiles~:Defaults.projectDefault~:Files.content) ++
        let projcusts = ctxproj~:coreFiles~:Defaults.projectOverwrites in
            concat$ projcusts>~Files.content
