{-# OPTIONS_GHC -Wall #-}

module Proj where

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
    Setup { srcRaw=srclines, tVal=tvalue }
    where
        tvalue key defval =
            Data.Map.Strict.findWithDefault defval key tsection
        tsection = Data.Map.Strict.fromList $
            srclines ~> map (tpersplit . (Util.splitBy ':'))
                ~> (filter $ not.null.fst)
            where
                tpersplit ("T":"":tname:tvalsplits) =
                    ( tname~>Util.trim , tvalsplits ~> (Util.join ":") ~> Util.trim ~> srcparsestr )
                tpersplit _ =
                    ( "" , "" )
        srclines =
            (Util.splitUp [reverse "{'("] (")'}") rawsrc) ~> map perchunk ~> concat ~> lines where
                --  join primary project file with additionally-specified 'overwrites' one:
                rawsrc =
                    (ctx~>coreFiles~>ProjDefaults.projectDefault~>Files.content) ++
                        let prjoverwrites = (ctx~>coreFiles~>ProjDefaults.projectOverwrites) in
                            if prjoverwrites==Files.NoFile then "" else prjoverwrites~>Files.content
                --  we splitUp above to turn all {'{multi-line
                --  fragments}'} into single-line '''Text.Read-able''' ones:
                perchunk (str,beg) =
                    if null beg then str else
                        ("'''" ++(show str ~> Util.dropLast 1 ~> drop 1)++ "'''")
        srcparsestr str
            | Util.startsWith str "'''" && Util.endsWith str "'''"
                = case (Text.Read.readMaybe $ "\"" ++(str ~> Util.dropLast 3 ~> drop 3)++ "\"") :: Maybe String of
                    Nothing -> str ; Just parsed -> parsed
            | otherwise
                = str
