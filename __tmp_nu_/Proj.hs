{-# OPTIONS_GHC -Wall #-}
module Proj where

import qualified Bloks
import qualified Defaults
import qualified Files
import qualified ProjC
import qualified ProjT
import qualified Util
import Util ( (~:) , (>~) , (~?) , (~!) )
import qualified X

import qualified Tmpl

import qualified Data.Map.Strict
import System.FilePath ( (</>) )



--  project context
data Ctx
    = ProjContext {
        projName :: String,
        setupName :: String,
        domainName :: String,
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



loadCtx ctxmain projname defaultfiles =
    let loadedsetup = _loadSetup ctxproj
        dirpath = ctxmain~:Files.dirPath
        dirpathjoin = (dirpath </>)
        setupname = ctxmain~:Files.setupName
        ctxproj = ProjContext {
            projName = projname,
            setupName = setupname,
            domainName = Util.ifNull (loadedsetup~:cfg~:ProjC.domainName) projname,
            dirPath = dirpath,
            dirPathBuild = dirpathjoin$
                setupname ++"-"++ loadedsetup~:cfg~:ProjC.dirNameBuild,
            dirPathDeploy = let dd = loadedsetup~:cfg~:ProjC.dirNameDeploy
                            in (null dd) ~? "" ~! dirpathjoin $setupname++"-"++dd,
            setup = loadedsetup,
            coreFiles = defaultfiles
        }
    in return ctxproj



_loadSetup ctxproj =
    SetupFromProj { -- srcRaw = srclinespost, srcPre = srclinesprep,
                    bloks = blokspost,
                    cfg = cfgpost,
                    tmpl = Tmpl.Processing {
                            Tmpl.bTags =  Bloks.tagResolver blokspost,
                            Tmpl.cTags = ProjC.tagResolver cfgmiscpost,
                            Tmpl.tTags = ProjT.tagResolver ttagspost,
                            Tmpl.xTags = X.tagResolver xtagspost,
                            Tmpl.processTags = cfgpost~:ProjC.tmplTags
                        }
                    }
    where
    setupprep = SetupFromProj { -- srcRaw = [], srcPre = [],
                                bloks = bloksprep,
                                cfg = cfgprep,
                                tmpl = Tmpl.Processing {
                                        Tmpl.bTags =  Bloks.tagResolver bloksprep,
                                        Tmpl.cTags = ProjC.tagResolver cfgmiscprep,
                                        Tmpl.tTags = ProjT.tagResolver ttagsprep,
                                        Tmpl.xTags = X.tagResolver xtagsprep,
                                        Tmpl.processTags = cfgprep~:ProjC.tmplTags
                                    }
                                }
    bloksprep = Bloks.parseProjLines preplinessplits
    blokspost = Bloks.parseProjLines postlinessplits
    (cfgprep,cfgmiscprep) = ProjC.parseProjLines preplinessplits
    (cfgpost,cfgmiscpost) = ProjC.parseProjLines postlinessplits
    ttagsprep = ProjT.parseProjLines preplinessplits False
    ttagspost = ProjT.parseProjLines postlinessplits True
    xtagsprep = X.parseProjLines preplinessplits
    xtagspost = X.parseProjLines postlinessplits

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
