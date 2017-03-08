module X.Iterator where

import Base
import qualified Lst
import qualified Str

import qualified Posts
import qualified Proj
import qualified Tmpl
import qualified Util
import qualified X

import qualified Data.List
import qualified Data.Map.Strict



data Tag
    = Cfg {
        prefix :: String,
        suffix :: String,
        joinVia :: String,
        content :: String
    }
    | Args {
        over :: Iteration
    }
    deriving Read


data Iteration
    = Range Int Int
    | RangeSans Int Int [Int]
    | Values [String]
    | BlokNames
    | FeedNames Bool
    | FeedValues Posts.Query String
    | FeedPosts Posts.Query [String]
    | But Tweak Iteration
    | With Iteration [Tweak]
    deriving Read

data SortOrder
    = None
    | Ascending
    | Descending
    | Shuffle Bool
    deriving (Eq, Read)

data Tweak
    = WrapEachIn (String , String)
    | LimitTo Int
    | Skip Int
    | Ordered SortOrder
    | Dyn [String]
    | BlokCat String
    deriving Read


registerX ctxproj xreg =
    let

    renderer (maybectxpage , argstr) =
        if waitforpage then Nothing
            else Just$ cfgwrap allcontents
        where
        allcontents = cfgjoin (iteratees >~ foreach)
        foreach | null cfgcontent = snd
                | otherwise = \ (i,v) -> cfgcontenter (show i , v , dyns@!i , show$ i+1 , num)

        num = show numtotal
        numtotal = iteratees~>length
        dyns = null dyns'' |? [] |! dyns' ++ repeat dlast where
            dlast = dyns'@!ilast ; ilast = dyns''~>length - 1
            dyns' = [0..ilast]>~g where g i = (null v && i > 0) |? g (i-1) |! v where v = dyns''@!i
            dyns'' = d$args-:over where d (With i t) = d (_w2b i t) ; d (But (Dyn vals) _) = vals ; d (But _ deeper) = d deeper ; d _ = []
        blokcat = bc (args-:over) where bc (But (BlokCat bcat) _) = bcat ; bc (But _ deeper) = bc deeper ; bc _ = ""
        iteratees = Lst.indexed (iter $args-:over) where

            --  RECURSIVE TWEAK-OPS:
            iter (With deeper tweaks) =
                iter (_w2b deeper tweaks)
            iter (But (WrapEachIn (pref , suff)) deeper) =
                (iter deeper) >~ ((pref++).(++suff))
            iter (But (LimitTo limit) deeper) =
                take limit (iter deeper)
            iter (But (Skip skip) deeper) =
                drop skip (iter deeper)
            iter (But (Ordered None) deeper) =
                iter deeper
            iter (But (Ordered (Shuffle perpage)) deeper) =
                shuffle perpage (iter deeper)
            iter (But (Ordered Descending) deeper) =
                (s deeper) (iter deeper) where
                    s (FeedPosts _ _) = id ; s _ = Util.sortDesc
            iter (But (Ordered Ascending) deeper) =
                (s deeper) (iter deeper) where
                    s (FeedPosts _ _) = reverse ; s _ = Data.List.sortBy compare
            iter (But _ deeper) =
                iter deeper

            --  ACTUAL ENUMERATIONS:
            iter (Values values) =
                values
            iter (RangeSans from to sans) =
                let range | (from > to) = reverse [to..from] | otherwise = [from..to]
                in (null sans |? range |! ( range ~| not.(`elem` sans) )) >~ show
            iter (Range from to) =
                iter (RangeSans from to [])
            iter BlokNames =
                projbloknames
            iter (FeedNames bloks) =
                (not bloks) |? projfeednames |! (projfeednames ++ projbloknames)
            iter (FeedValues query fieldname) =
                feedgroups query fieldname
            iter (FeedPosts query more) =
                (feedposts query blokcat (moreFromHtmlSplit more))
                    ~> preSorted >~ ((postFieldsToPairs more) ~. outputFeedPosts)
        (feedgroups,feedposts) = feedFuncs ctxproj maybectxpage
        args = X.tryParseArgs xreg ("over="++argstr) Nothing errargs where
            errargs = Args { over = Values [X.htmlErr$ X.clarifyParseArgsError (xreg , (Str.teaser 23 argstr))] }

        shuffle perpage =
            Lst.shuffleExtra (rndseeds maybectxpage perpage)
        rndseeds (Just pagectx) True = (pagectx-:Tmpl.randSeed) ++ (rndseeds Nothing False)
        rndseeds _ _ = ctxproj-:Proj.setup-:Proj.randSeed

        waitforpage =
            (not$ hasctxpage maybectxpage) && (needpage4iter $args-:over)
            where
            hasctxpage Nothing = False ; hasctxpage (Just _) = True
            needpage4iter (With iter buts) = needpage4iter (_w2b iter buts)
            needpage4iter (But (Ordered (Shuffle perpage)) deeper) = perpage || needpage4iter deeper
            needpage4iter (But _ deeper) = needpage4iter deeper
            needpage4iter (FeedValues query _) = needpage4feed query
            needpage4iter (FeedPosts query _) = needpage4feed query
            needpage4iter _ = False
            needpage4feed (Posts.Some feednames@(_:_) _ _) =
                not$ all (`elem` projfeednames) feednames
            needpage4feed _ =
                has projbloknames

    in X.EarlyOrWait renderer
    where
    projbloknames = Data.Map.Strict.keys (ctxproj-:Proj.setup-:Proj.bloks)
    projfeednames = ctxproj-:Proj.setup-:Proj.feeds
    cfgcontent = cfg-:content
    cfgcontenter (si,v,sd,sn,num) =
        cfgcontentsplits >>= persplit
        where
        persplit Si = si
        persplit V = v
        persplit Sd = sd
        persplit Sn = sn
        persplit Num = num
        persplit (Str str) = str

    cfgcontentsplits = (Util.splitUp id ["{:"] ":}" cfgcontent) >~ forsplit where
        forsplit (('i':[]) , ('{':_)) = Si
        forsplit (('v':[]) , ('{':_)) = V
        forsplit (('d':[]) , ('{':_)) = Sd
        forsplit (('n':[]) , ('{':_)) = Sn
        forsplit (('l':[]) , ('{':_)) = Num
        forsplit (othercontent, ('{':_)) = Str ('{':':':othercontent++":}")
        forsplit (othercontent, _) = Str othercontent
    cfgjoin = if null $cfg-:joinVia then concat else Lst.joined $cfg-:joinVia
    cfgwrap | (null $cfg-:prefix) && (null $cfg-:suffix) = id
            | otherwise = (((cfg-:prefix))++).(++((cfg-:suffix)))
    cfg_parsestr = Tmpl.fixParseStr "content" (xreg-:X.cfgFullStr)
    cfg = X.tryParseCfg xreg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { prefix = "" , suffix = "" , joinVia = ", " , content = "" }
        errcfg = Cfg { prefix = X.htmlErr (X.clarifyParseCfgError xreg) , suffix = "" , joinVia = "" , content = "" }
data WhyDaLuckyStiff = Si | V | Sd | Sn | Num | Str String


_w2b iter tweaks =
    w2b (reverse tweaks) where w2b [] = iter ; w2b (this:nested) = But this (w2b nested)


feedFuncs ctxproj maybectxpage =
    (ff Posts.feedGroups , ff Posts.feedPosts) where
        ff f = f maybectxbuild projposts projbloks
        projbloks = ctxproj-:Proj.setup-:Proj.bloks
        projposts = ctxproj-:Proj.setup-:Proj.posts
        maybectxbuild = maybectxpage >~ \ctxpage ->
                                            Posts.BuildContext (ctxpage-:Tmpl.lookupCachedPageRender)
                                                                (ctxpage-:Tmpl.allPagesFiles) projbloks
                                                                projposts (ctxproj-:Proj.setup-:Proj.cfg)


postFieldsToPairs more post =
    morefields ++ (Posts.wellKnownFields >~ (>~ (post-:)))
    where
    morefields = more >=~ topair
    topair mf =
        Lst.lookup mf (Posts.more post) >~ \val -> (mf , val)


moreFromHtmlSplit more =
    (more ~| is) >=~ Posts.moreFromHtmlSplit where
        is ('<':val) = elem '>' val ; is _ = False


outputFeedPosts =
    (Lst.crop 1 1) . show


preSorted posts =
    Data.List.sortBy presort posts -- to `vars` string, then snip off `[` and `]`
    where
    presort p1 p2 | (cdt==EQ) = compare p1 p2 | (otherwise) = cdt where
        cdt = compare (p2-:Posts.dt) (p1-:Posts.dt)
