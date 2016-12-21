{-# OPTIONS_GHC -Wall #-}
module Proj where

import qualified Files

import qualified System.FilePath



--  project context
data Ctx = Ctx {
        dir :: String,
        name :: String,
        defs :: Files.Defaults
    } deriving (Show)



loadCtx dirpath deffiles = let
        ctx = Ctx {
                dir = dirpath,
                name = System.FilePath.takeBaseName dirpath,
                defs = deffiles
            }
    in return ctx
