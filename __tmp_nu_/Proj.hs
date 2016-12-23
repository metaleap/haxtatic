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
    tVal :: String->String->String
}



loadCtx mainctx projname defaultfiles =
    let ctx = Ctx {
            name = projname,
            coreFiles = defaultfiles,
            setup = loadCoreFiles ctx
        }
    in ctx



loadCoreFiles ctx =
    let
        tvalue key defval =
            Data.Map.Strict.findWithDefault defval key tsection
    in
        Setup { srcRaw=srclines, tVal=tvalue }
    where
        srclines = _srclines_mlexpanded ctx
        tsection = Data.Map.Strict.fromList$
            srclinessplits ~> map tpersplit
                ~> (filter$ not.null.fst)
            where
                tpersplit ("T":"":tname:tvalsplits) =
                    ( tname~>Util.trim , tvalsplits ~> (Util.join ":") ~> Util.trim ~> srcparsestr )
                tpersplit _ =
                    ( "" , "" )
                srcparsestr str
                    | Util.startsWith str "'''" && Util.endsWith str "'''"
                        = case (Text.Read.readMaybe$ "\"" ++(str ~> Util.dropLast 3 ~> drop 3)++ "\"") :: Maybe String of
                            Nothing -> str ; Just parsed -> parsed
                    | otherwise
                        = str
        srclinessplits = srclines>~ (Util.splitBy ':')


_srclines_mlexpanded ctx =
    --  original lines with {'{multi-line
    --  fragments}'} rewritten into {T{_Hx_MlRepl_n}} placeholders ..
    ((mlchunked>~fst) ~> concat ~> lines) ++
        --  .. plus additional `T::_Hx_MlRepl_n:origstr` lines
        --  appended, defining the original extracted&replaced multi-line fragments
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
        --  fragments}'} into single-line '''Text.Read-able''' ones,
        --  put into new T::key:value lines, with the original
        --  occurrence rewritten into {T{key}}
        perchunk (i , (str , "{'{")) =
            let tkey = "_Hx_MlRepl_"++(show i) in
            ( "{T{"++tkey++"}}" , (tkey , "'''" ++(show str ~> Util.dropLast 1 ~> drop 1)++ "'''"))
        perchunk (_ , (str , _)) =
            (str , ("" , ""))
