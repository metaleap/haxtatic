{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.FeedView where

import Base
import qualified Posts
import qualified Proj
import qualified Tmpl
import qualified Util
import qualified X
import qualified X.Iterator


data Tag
    = Cfg {
        xnameGroupHeading :: String,
        htmlTagFeed :: (String , Util.StringPairs),
        xnameFeedItem :: String,
        feedMore :: [String]
    }
    | Args {
        feeds :: [String],
        groups :: [Group]
    }
    deriving Read

data Group
    = G String [String] Posts.QueryDate
    deriving Read


registerX ctxproj xreg =
    let
    renderer (maybectxpage , argstr) =
        (Just . concat) $args-:groups>~foreach
        where
        foreach (G title cats dates) =
            let query = Posts.Some (args-:feeds) cats dates
                more = cfg-:feedMore
                posts = feedposts query "dt" (X.Iterator.moreFromHtmlSplit more)
                outitems = (X.Iterator.outputFeed posts more) >~ \ strvars ->
                            xfeeditem ("vars=["++strvars++"],content=>")
            in if null outitems then "" else
                concat (xgroups title : outitems)

        (_ , feedposts) = X.Iterator.feedFuncs ctxproj maybectxpage
        (xgroups , xfeeditem) = (x (cfg-:xnameGroupHeading) xerrgroups , x (cfg-:xnameFeedItem) xerrfeeditem) where
            x xn xerr v = xerr v -|= xtags maybectxpage (xn++':':v)
        args = X.tryParseArgs xreg argstr Nothing errval where
            errval = Args{ feeds=[], groups=[G errmsg [] Posts.Any] } where
                errmsg = X.htmlErr$ X.clarifyParseArgsError (xreg , (Util.excerpt 23 argstr))
    in X.Early renderer
    where
    (xerrgroups , xerrfeeditem) = (x $cfg-:xnameGroupHeading, x $cfg-:xnameFeedItem) where
        x xn = (++"|}") . (("{X|"++xn)++) . ((:) ':')
    xtags = ctxproj-:Proj.setup-:Proj.ctxTmpl-:Tmpl.xTagHandler
    cfg = X.tryParseCfg xreg (xreg-:X.cfgFullStr) Nothing errval where
        errval = Cfg { xnameGroupHeading = "", htmlTagFeed = ("div", X.htmlErrAttsCfg xreg), xnameFeedItem = "", feedMore = [] }
