{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.FeedView where

import Base
import qualified Html
import qualified Posts
import qualified Proj
import qualified Tmpl
import qualified Util
import qualified X
import qualified X.FormatDateTime
import qualified X.Iterator

import qualified Data.List


data Tag
    = Cfg {
        xnameGroupHeading :: String,
        feedWrap :: (String , String),
        xnameFeedItem :: String,
        feedMore :: [String]
    }
    | Args {
        feeds :: [String],
        groups :: [GroupWithNameCatsDates],
        xVars :: [(String , XVar)]
    }
    deriving Read


data GroupWithNameCatsDates
    = Group String [String] Posts.QueryDate
    deriving Read


data XVar
    = F String
    | V String
    | DtFormat String XVar
    | OneOf [XVar]
    | FeedWise String XVar XVar
    | Format String [XVar]
    | PrefixIf String XVar
    | StripMarkup XVar
    | Wrap String String XVar
    | X XVar
    deriving Read



registerX ctxproj xreg =
    let
    renderer (maybectxpage , argstr) =
        Just$ concat$ allgroups>~foreach
        where
        allgroups = if has (args-:groups) then (args-:groups)
                    else ((feedgroups (postsfrom [] Posts.AnyDate) "dt:year") ~> (Data.List.sortBy (flip compare))) >~ togroup
        togroup year = Group year [] (Posts.Between year ((show . (+ 1)) (Util.tryParseOr 9998 year)))
        foreach (Group title cats dates) =
            let query = postsfrom cats dates
                more = cfg-:feedMore
                posts = X.Iterator.preSorted$ feedposts query "dt" (X.Iterator.moreFromHtmlSplit more)
                postsfields = posts >~ X.Iterator.postFieldsToPairs more
                outxvars = postsfields >~ tooutitem
                outitems = outxvars >~ X.Iterator.outputFeedPosts >~ \ strvars ->
                            xfeeditem ("vars=["++strvars++"],content=>")
            in if null outitems then "" else
                (xgroups title) ++ (feedwrap (concat outitems))

        tooutitem fieldpairs =
            xvhandlers >~ \(xvname , handler) -> (xvname , handler fieldpairs)

        xvhandlers =
            (args-:xVars) >~ \(xvname , xv) -> (xvname , tofunc xv) where
                tofunc (V val) = const val
                tofunc (F name) = Util.lookup name ""
                tofunc (Wrap pref suff xv) = (pref++) . (++suff) . (tofunc xv)
                tofunc (StripMarkup xv) = (Html.stripMarkup False ' ') . (tofunc xv)
                tofunc (PrefixIf p xv) = (ifis (p++)) . (tofunc xv) where
                    ifis _ "" = "" ; ifis fn val = fn val
                tofunc (OneOf []) = const "" ; tofunc (OneOf [xv]) = tofunc xv
                tofunc (OneOf (xv:xvs)) = retry (tofunc xv) (tofunc (OneOf xvs)) where
                    retry _ _ [] = [] ; retry fmay fnay val = has s |? s |! fnay val where s = fmay val
                tofunc (Format text xvs) = ((-|=) text) . Util.formatWithList text . vals xvs where
                    vals _ [] = [] ; vals [] _ = [] ; vals (xv:rest) fp = tofunc xv fp : vals rest fp
                tofunc (FeedWise feed yay nay) = switch . my "feed" where
                    my = ((,) <*>) . l where l k = ("" -|=) . Data.List.lookup k
                    switch (carry,v2) = tofunc (feed==v2 |? yay |! nay) carry
                tofunc (DtFormat dtfname xv) = (try (dtformat dtfname)) . (tofunc xv)
                tofunc (X xv) = (try (xtags maybectxpage)) . (tofunc xv)
                try _ [] = [] ; try maybeer val = case maybeer val of Nothing -> val ; Just v -> v

        postsfrom = Posts.Some (args-:feeds)
        (feedgroups , feedposts) = X.Iterator.feedFuncs ctxproj maybectxpage
        (xgroups , xfeeditem) = (x (cfg-:xnameGroupHeading) xerrgroups , x (cfg-:xnameFeedItem) xerrfeeditem) where
            x xn xerr v = xerr v -|= xtags maybectxpage (xn++':':v)
        args = X.tryParseArgs xreg argstr Nothing errval where
            errval = Args{ feeds=[], groups=[Group errmsg [] Posts.AnyDate], xVars = [] } where
                errmsg = X.htmlErr$ X.clarifyParseArgsError (xreg , (Util.excerpt 23 argstr))
    in X.WaitForPage renderer
    where
    (xerrgroups , xerrfeeditem) = (x $cfg-:xnameGroupHeading, x $cfg-:xnameFeedItem) where
        x xn = (++"|}") . (("{X|"++xn)++) . ((:) ':')
    dtformat = X.FormatDateTime.dtFormatter ctxproj ""
    feedwrap = ((fst $cfg-:feedWrap)++).(++(snd $cfg-:feedWrap))
    xtags = ctxproj-:Proj.setup-:Proj.ctxTmpl-:Tmpl.xTagHandler
    cfg = X.tryParseCfg xreg (xreg-:X.cfgFullStr) Nothing errval where
        errval = Cfg { xnameGroupHeading = "", xnameFeedItem = "", feedMore = [],
                        feedWrap = ("<div>" ++ (X.htmlErr$ X.clarifyParseCfgError xreg), "</div>") }
