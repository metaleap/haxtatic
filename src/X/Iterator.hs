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
    | Values [String]
    | BlokNames
    | FeedNames Bool
    | FeedValues FeedSelect String
    | FeedPosts FeedSelect
    | But Tweak Iteration
    | With Iteration [Tweak]
    deriving Read

data FeedSelect
    = Pick Posts.Query String [String]
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
    deriving Read


registerX ctxproj xreg =
    let

    renderer (maybectxpage , argstr) =
        if waitforpage then Nothing
            else Just$ cfgwrap allcontents
        where
        allcontents = cfgjoin (iteratees >~ foreach)

        iteratees = Util.indexed (iter $args-:over) where

            --  RECURSIVE TWEAK OPS:
            iter (With moreover tweaks) =
                iter (_with2buts moreover tweaks)
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
                (s moreover) (iter moreover) where -- feed stuff comes pre-sorted descending, so:
                    s (FeedValues _ _) = id ; s (FeedPosts _) = id ; s _ = Data.List.sortBy (flip compare)
            iter (But (Ordered Ascending) moreover) =
                (s moreover) (iter moreover) where -- feed stuff comes pre-sorted descending, so:
                    s (FeedValues _ _) = reverse ; s (FeedPosts _) = reverse ; s _ = Data.List.sortBy compare

            --  ACTUAL ENUMERATIONS:
            iter (Values values) =
                values
            iter (Range from to) =
                let range | (from > to) = reverse [to..from] | otherwise = [from..to]
                in range >~ show
            iter BlokNames =
                projbloknames
            iter (FeedNames bloks) =
                (not bloks) |? projfeednames |! (projfeednames ++ projbloknames)
            iter (FeedValues (Pick query dtformat more) fieldname) =
                case Data.List.lookup fieldname (Posts.wellKnownFields True) of
                    Nothing -> []
                    Just field -> feedgroups query dtformat ((morefromhtml more)>=~Posts.moreFromHtmlSplit) field
            iter (FeedPosts (Pick query dtformat more)) =
                (feedposts query dtformat ((morefromhtml more)>=~Posts.moreFromHtmlSplit))
                    >~ (fields2pairs ~. show ~. (Util.crop 1 1))
                where
                fields2pairs post =
                    ((Posts.wellKnownFields False) ++ morefields) >~ (Util.both (id , (post-:)))
                morefields = more >~ topair where
                    topair mfield =
                        mfield =: Posts.more~.(Util.lookup mfield $"{!|"++mfield++"|!}")
        (feedgroups,feedposts) = (ff Posts.feedGroups , ff Posts.feedPosts) where ff f = f maybectxbuild projposts projbloks
        morefromhtml more = more ~| is where is ('<':val) = elem '>' val ; is _ = False
        maybectxbuild = maybectxpage =>- \ctxpage -> Posts.BuildContext (ctxpage-:Tmpl.lookupCachedPageRender)
                                                                        (ctxpage-:Tmpl.allPagesFiles) projbloks
                                                                        projposts (ctxproj-:Proj.setup-:Proj.cfg)
        args = X.tryParseArgs xreg ("over="++argstr) Nothing errargs where
            errargs = Args { over = Values [X.htmlErr$ X.clarifyParseArgsError (xreg , (Util.excerpt 23 argstr))] }

        shuffle perpage =
            Util.times 3 (Util.shuffleExtra (randseeds maybectxpage perpage))
        randseeds (Just pagectx) True =
            (pagectx-:Tmpl.randSeed) ++ (randseeds Nothing False)
        randseeds _ _ =
            ctxproj-:Proj.setup-:Proj.randSeed

        waitforpage =
            (not$ hasctxpage maybectxpage) && (needpage4iter $args-:over)
            where
            hasctxpage Nothing = False ; hasctxpage (Just _) = True
            needpage4iter (With iter buts) = needpage4iter (_with2buts iter buts)
            needpage4iter (But (Ordered (Shuffle perpage)) moreover) = perpage || needpage4iter moreover
            needpage4iter (But _ moreover) = needpage4iter moreover
            needpage4iter (FeedValues (Pick query _ _) _) = needpage4feed query
            needpage4iter (FeedPosts (Pick query _ _)) = needpage4feed query
            needpage4iter _ = False
            needpage4feed (Posts.Filter feednames@(_:_) _ _) =
                not$ all (`elem` projfeednames) feednames
            needpage4feed _ =
                has projbloknames

        projposts = ctxproj-:Proj.setup-:Proj.posts
        projfeednames = ctxproj-:Proj.setup-:Proj.feeds
    in X.EarlyOrWait renderer
    where

    projbloks = ctxproj-:Proj.setup-:Proj.bloks
    projbloknames = Data.Map.Strict.keys projbloks
    foreach | (null txt)=   \ (_,v) -> v
            | (otherwise)=  _repl (hasi,hasv) txt
            where txt = cfg-:content ; hasi = _hasi txt ; hasv = _hasv txt
    cfgjoin = if null $cfg-:joinVia then concat else Util.join $cfg-:joinVia
    cfgwrap | (null $cfg-:prefix) && (null $cfg-:suffix) = id
            | otherwise = (((cfg-:prefix))++).(++((cfg-:suffix)))
    cfg_parsestr = Tmpl.fixParseStr "content" (xreg-:X.cfgFullStr)
    cfg = X.tryParseCfg xreg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { prefix = "" , suffix = "" , joinVia = ", " , content = "" }
        errcfg = Cfg { prefix = X.htmlErr (X.clarifyParseCfgError xreg) , suffix = "" , joinVia = "" , content = "" }



_with2buts iter tweaks =
    w2b (reverse tweaks) where w2b [] = iter ; w2b (this:nested) = But this (w2b nested)

_repl   (False,False)   txt                         _               = txt
_repl   _               []                          _               = []
_repl   iv@(True,_)     ('{':'%':'i':'%':'}':rest)  both@(i , _)    = (show i) ++ _repl iv rest both
_repl   iv@(_,True)     ('{':'%':'v':'%':'}':rest)  both@(_ , v)    = v ++ _repl iv rest both
_repl   iv              (this : rest)               both            = this : _repl iv rest both

_hasi [] = False ; _hasi ('{':'%':'i':'%':'}':_) = True ; _hasi (_:rest) = _hasi rest
_hasv [] = False ; _hasv ('{':'%':'v':'%':'}':_) = True ; _hasv (_:rest) = _hasv rest
