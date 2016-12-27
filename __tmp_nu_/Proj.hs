{-# OPTIONS_GHC -Wall #-}

module Proj where

import qualified Bloks
import qualified Files
import qualified ProjDefaults
import qualified ProjTxts
import qualified Util
import Util ( (~>) , (>~) , (~.) )

import qualified Data.Map.Strict
import qualified Text.Read



--  project context
data Ctx = Ctx {
        name :: String,
        coreFiles :: ProjDefaults.CoreFiles,
        setup :: Setup
    }

data Setup = Setup {
    srcRaw :: [String],
    srcPre :: [String],
    tTags :: String->String,
    bTags :: String->String
}



loadCtx mainctx projname defaultfiles =
    let ctx = Ctx {
            name = projname,
            coreFiles = defaultfiles,
            setup = loadCoreFiles ctx
        }
    in ctx



loadCoreFiles ctx =
    let setuppost = Setup { srcRaw=srclinespost, srcPre=srclinesprep,
                            tTags=tvalpost,
                            bTags= Bloks.bTagResolver "" blokspost }
    in setuppost
    where
        setuppre = Setup { srcRaw=[], srcPre=[],
                            tTags=tvalpre,
                            bTags= Bloks.bTagResolver "" blokspre }

        blokspre = Bloks.parseDefs preplinessplits
        blokspost = Bloks.parseDefs postlinessplits

        tvalpre = ProjTxts.parseDefs preplinessplits False
        tvalpost = ProjTxts.parseDefs postlinessplits True

        srclinesprep = ProjTxts.srcLinesExpandMl$ _rawsrc ctx
        srclinespost = processSrcFully setuppre (srclinesprep~>unlines) ~> lines
        preplinessplits = srclinesprep>~ _splitc
        postlinessplits = srclinespost>~ _splitc
        _splitc = Util.splitBy ':'



processSrcFully =
    Util.repeatedly . processSrcJustOnce

processSrcJustOnce ctxSetup src =
    ((Util.splitUp ["{T{","{B{"] "}}" src)>~perchunk) ~> concat
    where
        perchunk (str , "{B{") =
            (ctxSetup~>bTags) str
        perchunk (str , "{T{") =
            (ctxSetup~>tTags) str
        perchunk (str , _) =
            str



_rawsrc ctx =
    --  join primary project file with additionally-specified 'overwrites' one:
    (ctx~>coreFiles~>ProjDefaults.projectDefault~>Files.content) ++
        let prjoverwrites = (ctx~>coreFiles~>ProjDefaults.projectOverwrites) in
            if prjoverwrites==Files.NoFile then "" else prjoverwrites~>Files.content
