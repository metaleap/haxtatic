{-# OPTIONS_GHC -Wall #-}

module Proj where

import qualified Bloks
import qualified Files
import qualified ProjCfg
import qualified ProjDefaults
import qualified ProjTxts
import qualified Util
import Util ( (~:) , (>~) )

import qualified Data.Map.Strict
import System.FilePath ( (</>) )



--  project context
data Ctx = Ctx {
        projName :: String,
        setupName :: String,
        dirPath :: FilePath,
        dirPathBuild :: FilePath,
        dirPathDeploy :: FilePath,
        setup :: Setup,
        coreFiles :: ProjDefaults.CoreFiles
    }

data Setup = Setup {
    --  srcRaw :: [String],
    --  srcPre :: [String],
    bloks :: Data.Map.Strict.Map String Bloks.Blok,
    cfg :: ProjCfg.Cfg,
    tTags :: String->String,
    bTags :: String->String
}




loadCtx ctxmain projname defaultfiles =
    let loadedsetup = _loadSetup ctxproj
        dirpath = ctxmain~:Files.dirPath
        dirpathjoin = (dirpath </>)
        setupname = ProjDefaults.setupName $defaultfiles~:ProjDefaults.projectDefault~:Files.path
        ctxproj = Ctx {
            projName = projname,
            setupName = setupname,
            dirPath = dirpath,
            dirPathBuild = dirpathjoin $setupname++"-"++loadedsetup~:cfg~:ProjCfg.dirNameBuild,
            dirPathDeploy = let dd = loadedsetup~:cfg~:ProjCfg.dirNameDeploy
                            in if null dd then "" else dirpathjoin $setupname++"-"++dd,
            setup = loadedsetup,
            coreFiles = _loadCoreFiles loadedsetup defaultfiles
        }
    in return ctxproj


_loadCoreFiles projsetup deffiles =
    ProjDefaults.rewriteTemplates deffiles tmplrewriter where
        tmplrewriter = processSrcFully projsetup


_loadSetup ctxproj =
    let setuppost = Setup { -- srcRaw = srclinespost, srcPre = srclinesprep,
                            bloks = blokspost,
                            cfg = cfgpost,
                            tTags = ttagspost,
                            bTags =  Bloks.bTagResolver "" blokspost }
    in setuppost
    where
        setupprep = Setup { -- srcRaw = [], srcPre = [],
                            bloks = bloksprep,
                            cfg = cfgprep,
                            tTags = ttagsprep,
                            bTags =  Bloks.bTagResolver "" bloksprep }

        bloksprep = Bloks.parseDefs preplinessplits
        blokspost = Bloks.parseDefs postlinessplits
        cfgprep = ProjCfg.parseDefs preplinessplits
        cfgpost = ProjCfg.parseDefs postlinessplits
        ttagsprep = ProjTxts.parseDefs preplinessplits False
        ttagspost = ProjTxts.parseDefs postlinessplits True

        preplinessplits = srclinesprep>~ _splitc
        postlinessplits = srclinespost>~ _splitc
        _splitc = Util.splitBy ':'
        srclinesprep = ProjTxts.srcLinesExpandMl$ _rawsrc ctxproj
        srclinespost = processSrcFully setupprep (srclinesprep~:unlines) ~: lines



processSrcFully =
    Util.repeatedly . processSrcJustOnce

processSrcJustOnce ctxsetup src =
    ((Util.splitUp ["{T{","{B{"] "}}" src)>~perchunk) ~: concat
    where
        perchunk (str , "{B{") =
            (ctxsetup~:bTags) str
        perchunk (str , "{T{") =
            (ctxsetup~:tTags) str
        perchunk (str , _) =
            str



_rawsrc ctxproj =
    --  join primary project file with additionally-specified 'overwrites' one:
    (ctxproj~:coreFiles~:ProjDefaults.projectDefault~:Files.content) ++
        let prjoverwrites = (ctxproj~:coreFiles~:ProjDefaults.projectOverwrites) in
            if prjoverwrites==Files.NoFile then "" else prjoverwrites~:Files.content
