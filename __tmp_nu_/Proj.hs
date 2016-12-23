{-# OPTIONS_GHC -Wall #-}

module Proj where

import qualified Bloks
import qualified Files
import qualified ProjDefaults
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

        tvalpre = tvalue (tsection preplinessplits False)
        tvalpost = tvalue (tsection postlinessplits True)
        tvalue hashmap key =
            Data.Map.Strict.findWithDefault ("{!T{"++key++"}!}") key hashmap

        srclinesprep = _srclines_expandml ctx
        srclinespost = processSrcFully setuppre (srclinesprep~>unlines) ~> lines
        tsection linessplits canparsestr = Data.Map.Strict.fromList$
            linessplits ~> map tpersplit
                ~> (filter $not.null.fst)
            where
                tpersplit ("T":"":tname:tvalsplits) =
                    ( tname~>Util.trim , tvalsplits ~> (Util.join ":") ~> Util.trim ~> srcparsestr )
                tpersplit _ =
                    ( "" , "" )
                srcparsestr str
                    | canparsestr && Util.startsWith str "\"" && Util.endsWith str "\""
                        = case (Text.Read.readMaybe str) :: Maybe String of
                            Nothing -> str ; Just parsed -> parsed
                    | otherwise
                        = str
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


_srclines_expandml ctx =
    --  original lines exposing {'{multi-line
    --  fragments}'} collapsed into single-line in-place {T{_Hx_MlRepl_n}} placeholders ..
    ((mlchunked>~fst) ~> concat ~> lines) ++
        --  .. plus additional `T::_Hx_MlRepl_n:"original-but-\n-escaped-and-quoted"`
        --  lines appended, supplying the original extracted&replaced multi-line fragments
        (mlchunked>~ snd~.mlwriteln)
    where
    mlwriteln ("",_) = ""
    mlwriteln (k,v) = "T::"++k++":"++v
    mlchunked = mlchunks>~perchunk where
        mlchunks = Util.indexed$ Util.splitUp ["{'{"] "}'}" rawsrc where
            --  join primary project file with additionally-specified 'overwrites' one:
            rawsrc =
                (ctx~>coreFiles~>ProjDefaults.projectDefault~>Files.content) ++
                    let prjoverwrites = (ctx~>coreFiles~>ProjDefaults.projectOverwrites) in
                        if prjoverwrites==Files.NoFile then "" else prjoverwrites~>Files.content
        --  we splitUp above in order to now turn all {'{multi-line
        --  fragments}'} into single-line "Text.Read-able" ones,
        --  put into new T::key:value lines, with the original
        --  occurrence rewritten into {T{key}}
        perchunk (i , (str , "{'{")) =
            let tkey = "_Hx_MlRepl_"++(show i) in
            ( "{T{"++tkey++"}}" , (tkey , "\"" ++(show str ~> Util.dropLast 1 ~> drop 1)++ "\""))
        perchunk (_ , (str , _)) =
            (str , ("" , ""))
