module X.FeedView where

import Base
import qualified Lst

import qualified Html
import qualified Posts
import qualified Proj
import qualified Tmpl
import qualified Util
import qualified X
import qualified X.FormatDateTime
import qualified X.Iterator

import qualified Data.Map.Strict


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
        if waitforpage then Nothing
            else Just$ allgroups >>= render
        where
        allgroups = (args-:groups) <?>  --  if no groups defined $ fall back to: feed years, descending
                        ((Util.sortDesc$ feedgroups (postsfrom [] Posts.AnyDate) "dt:year") >~ togroup) where
                            togroup year = Group year [] (Posts.Between year (show$ 1 + Util.tryParseOr (9998::Int) year))
        render (Group title cats dates) =
            null outitems |? "" |! xgroups title ++ cfgfeedwrap outitems
            where
            outitems = posts >~ X.Iterator.postFieldsToPairs more ~. tooutitem ~. xfeedwrap
            posts = X.Iterator.preSorted$ feedposts query "dt" mh
            more = cfg-:feedMore ; mh = X.Iterator.moreFromHtmlSplit more
            query = postsfrom cats dates

        tooutitem fieldpairs =
            xvhandlers >~ (>~ ($fieldpairs))

        xvhandlers =
            (args-:xVars) >~ (>~ tofunc) where
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
                    my = ((,) <*>) . l where l k = ("" -|=) . Lst.lookup k
                    switch (carry,v2) = tofunc (feed==v2 |? yay |! nay) carry
                tofunc (DtFormat dtfname xv) = (try (dtformat dtfname)) . (tofunc xv)
                tofunc (X xv) = (try (xtags maybectxpage)) . (tofunc xv)
                try _ [] = [] ; try maybeer val = case maybeer val of Nothing -> val ; Just v -> v

        xfeedwrap = xfeeditem . ("vars=["++) . (++"],content=>") . X.Iterator.outputFeedPosts
        postsfrom = Posts.Some (args-:feeds)
        (feedgroups , feedposts) = X.Iterator.feedFuncs ctxproj maybectxpage
        (xgroups , xfeeditem) = xboth maybectxpage
        args = X.tryParseArgs xreg argstr Nothing errval where
            errval = Args{ feeds=[], groups=[Group errmsg [] Posts.AnyDate], xVars = [] } where
                errmsg = X.htmlErr$ X.clarifyParseArgsError (xreg , (Util.excerpt 23 argstr))
        waitforpage =
            (not$ hasctxpage maybectxpage) && (needpage4feeds $args-:feeds)
            where
            hasctxpage Nothing = False ; hasctxpage (Just _) = True
            needpage4feeds [] = has projbloknames
            needpage4feeds feednames = not$ all (`elem` projfeednames) feednames
    in X.EarlyOrWait renderer
    where

    projbloknames = Data.Map.Strict.keys (ctxproj-:Proj.setup-:Proj.bloks)
    projfeednames = ctxproj-:Proj.setup-:Proj.feeds

    xboth maybectxpage =
        (x (cfg-:xnameGroupHeading) xerrgroups , x (cfg-:xnameFeedItem) xerrfeeditem) where
            x xn xerr v = xerr v -|= xtags maybectxpage (xn++':':v)
    (xerrgroups , xerrfeeditem) = (x $cfg-:xnameGroupHeading, x $cfg-:xnameFeedItem) where
        x xn = (++"|}") . (("{X|"++xn)++) . ((:) ':')
    dtformat = X.FormatDateTime.dtFormatter ctxproj ""
    cfgfeedwrap = ((fst $cfg-:feedWrap)++) . (++(snd $cfg-:feedWrap)) . concat
    xtags = ctxproj-:Proj.setup-:Proj.ctxTmpl-:Tmpl.xTagHandler
    cfg = X.tryParseCfg xreg (xreg-:X.cfgFullStr) Nothing errval where
        errval = Cfg { xnameGroupHeading = "", xnameFeedItem = "", feedMore = [],
                        feedWrap = ("<div>" ++ (X.htmlErr$ X.clarifyParseCfgError xreg), "</div>") }
