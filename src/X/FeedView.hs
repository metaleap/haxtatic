{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.FeedView where

import Base
import qualified Posts
import qualified Util
import qualified X


data Tag
    = Args {
        feeds :: [String],
        groups :: [Group]
    }
    deriving Read

data Group
    = G String [String] Posts.QueryDate
    deriving Read


registerX _ctxproj xreg =
    let
    renderer (_maybectxpage , argstr) =
        Just$ concat$ args-:groups>~pergroup
        where
        pergroup (G title cats dates) =
            "<h2 id=\""++title++"\">"++title++"</h2>"

        args = X.tryParseArgs xreg argstr Nothing errval where
            errval = Args{ feeds=[], groups=[G (X.htmlErr$ X.clarifyParseArgsError (xreg , (Util.excerpt 23 argstr))) [] Posts.Any] }

    in X.Early renderer
    where
