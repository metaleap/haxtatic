{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Xrepeat where

import Base
import qualified Posts
import qualified Proj
import qualified Tmpl
import qualified Util
import qualified X

import qualified Data.List
import qualified Data.Map.Strict


data Iterate
    = Range Int Int
    | Values [String]
    | Bloks
    -- | Posts [String]
    deriving (Read)


data SortOrder
    = None
    | Ascending
    | Descending
    deriving (Eq, Read)


data Tag
    = Cfg {
        prefix :: String,
        suffix :: String,
        joinwith :: String,
        content :: String
    }
    | Args {
        over :: Iterate,
        order :: SortOrder
    } deriving (Read)


registerX ctxproj xreg =
    let
    renderer (_ , argstr) =
        Just$ (cfg-:prefix) ++ allcontents ++ (cfg-:suffix)
        where
        allcontents = Util.join (cfg-:joinwith) (iteratees >~ foreach)
        foreach (i,v) =
            Util.replaceSubs ["[:i:]" =: show i , "[:v:]" =: v] (cfg-:content)
        iteratees = Util.indexed$ case args-:over of
                        Values values -> ordered$ values
                        Range from to -> ordered$ [from..to] >~ show
                        Bloks -> ordered$ Data.Map.Strict.keys$ ctxproj-:Proj.setup-:Proj.bloks
        ordered = o (args-:order) where
            o Ascending = Data.List.sort
            o Descending = Data.List.sortBy (flip compare)
            o _ = id
        args = X.tryParseArgs argstr (Just defargs) errargs where
            defargs = Args { over = Values [], order = None }
            errargs = Args { over = Values [X.htmlErr$ X.clarifyParseArgsError (xreg , (Util.excerpt 23 argstr))], order = None }

    in X.Early renderer
    where

    cfg_parsestr = Tmpl.fixParseStr "content" (xreg-:X.cfgFullStr)
    cfg = X.tryParseCfg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { prefix = "" , suffix = "" , joinwith = "" , content = "" }
        errcfg = Cfg { prefix = X.htmlErr (X.clarifyParseCfgError xreg) , suffix = "" , joinwith = "" , content = "" }
