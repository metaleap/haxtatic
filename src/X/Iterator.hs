{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.Iterator where

import Base
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
        allcontents = cfgjoin (iteratees >~ (foreach dyns n)) where n = show numtotal

        numtotal = iteratees~>length
        dyns = d (args-:over) where d (With i t) = d (_w2b i t) ; d (But (Dyn vals) _) = vals ; d (But _ moreover) = d moreover ; d _ = []
        blokcat = bc (args-:over) where bc (But (BlokCat bcat) _) = bcat ; bc (But _ moreover) = bc moreover ; bc _ = ""
        iteratees = Util.indexed (iter $args-:over) where

            --  RECURSIVE TWEAK-OPS:
            iter (With moreover tweaks) =
                iter (_w2b moreover tweaks)
            iter (But (WrapEachIn (pref , suff)) moreover) =
                (iter moreover) >~ ((pref++).(++suff))
            iter (But (LimitTo limit) moreover) =
                take limit (iter moreover)
            iter (But (Skip skip) moreover) =
                drop skip (iter moreover)
            iter (But (Ordered None) moreover) =
                iter moreover
            iter (But (Ordered (Shuffle perpage)) moreover) =
                shuffle perpage (iter moreover)
            iter (But (Ordered Descending) moreover) =
                (s moreover) (iter moreover) where
                    s (FeedPosts _ _) = id ; s _ = Data.List.sortBy (flip compare)
            iter (But (Ordered Ascending) moreover) =
                (s moreover) (iter moreover) where
                    s (FeedPosts _ _) = reverse ; s _ = Data.List.sortBy compare
            iter (But _ moreover) =
                iter moreover

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
            errargs = Args { over = Values [X.htmlErr$ X.clarifyParseArgsError (xreg , (Util.excerpt 23 argstr))] }

        shuffle perpage =
            Util.shuffleExtra (rndseeds maybectxpage perpage)
        rndseeds (Just pagectx) True = (pagectx-:Tmpl.randSeed) ++ (rndseeds Nothing False)
        rndseeds _ _ = ctxproj-:Proj.setup-:Proj.randSeed

        waitforpage =
            (not$ hasctxpage maybectxpage) && (needpage4iter $args-:over)
            where
            hasctxpage Nothing = False ; hasctxpage (Just _) = True
            needpage4iter (With iter buts) = needpage4iter (_w2b iter buts)
            needpage4iter (But (Ordered (Shuffle perpage)) moreover) = perpage || needpage4iter moreover
            needpage4iter (But _ moreover) = needpage4iter moreover
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
    foreach dyn num | null cfgcontent   = \ (_,v) -> v    --  no content -> "{:v:}"
                    | otherwise         = repl cfgcontent
                    where
                    cfgcontent = cfg-:content
                    repl []                         _           = []
                    repl ('{':':':'i':':':'}':rest) iv@(i , _)  = (show i) ++ repl rest iv
                    repl ('{':':':'n':':':'}':rest) iv@(i , _)  = (show$ i+1) ++ repl rest iv
                    repl ('{':':':'v':':':'}':rest) iv@(_ , v)  = v ++ repl rest iv
                    repl ('{':':':'l':':':'}':rest) iv          = num ++ repl rest iv
                    repl ('{':':':'d':':':'}':rest) iv@(i , _)  = grab i ++ (repl rest iv)
                    repl (this : rest)              iv          = this : repl rest iv
                    grab i =
                        if has v || i == 0 then v else grab (i-1) where v = "" -|= (dyn@?i)
    cfgjoin = if null $cfg-:joinVia then concat else Util.join $cfg-:joinVia
    cfgwrap | (null $cfg-:prefix) && (null $cfg-:suffix) = id
            | otherwise = (((cfg-:prefix))++).(++((cfg-:suffix)))
    cfg_parsestr = Tmpl.fixParseStr "content" (xreg-:X.cfgFullStr)
    cfg = X.tryParseCfg xreg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { prefix = "" , suffix = "" , joinVia = ", " , content = "" }
        errcfg = Cfg { prefix = X.htmlErr (X.clarifyParseCfgError xreg) , suffix = "" , joinVia = "" , content = "" }



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
    morefields ++ (Posts.wellKnownFields >~ (fmap (post-:)))
    where
    morefields = more >=~ topair
    topair mf =
        Data.List.lookup mf (Posts.more post) >~ \val -> (mf , val)


moreFromHtmlSplit more =
    (more ~| is)>=~Posts.moreFromHtmlSplit where
        is ('<':val) = elem '>' val ; is _ = False


outputFeedPosts =
    (Util.crop 1 1) . show


preSorted posts =
    Data.List.sortBy presort posts -- to `vars` string, then snip off `[` and `]`
    where
    presort p1 p2 | (cdt==EQ) = compare p1 p2 | (otherwise) = cdt where
        cdt = compare (p2-:Posts.dt) (p1-:Posts.dt)
