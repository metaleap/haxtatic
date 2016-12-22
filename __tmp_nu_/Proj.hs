{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NegativeLiterals #-}

module Proj where

import Util ( ($>) )

import qualified Data.Map.Strict
import qualified ProjDefaults



--  project context
data Ctx = Ctx {
        name :: String,
        coreFiles :: ProjDefaults.CoreFiles,
        setup :: Setup,
        getTVal :: String->String->String
    }

data Setup = Setup {
    tSection :: Data.Map.Strict.Map String String
}



loadCtx mainctx projname defaultfiles =
    let ctx = Ctx {
                name = projname,
                coreFiles = defaultfiles,
                setup = loadFiles ctx
            }
    in return ctx


loadFiles ctx =
    Setup { tSection=tsection }
    where
        tsection = Data.Map.Strict.empty


tVal ctx key defval =
    Data.Map.Strict.findWithDefault defval key (ctx$>setup$>tSection)
