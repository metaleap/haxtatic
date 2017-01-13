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


data Tag
    = Cfg {
        prefix :: String,
        suffix :: String,
        joinwith :: String,
        content :: String
    }
    | Args {
        over :: Iterate,
        wrap :: Maybe (String , String),
        order :: SortOrder,
        skip :: Int,
        limit :: Int
    } deriving (Read)


data Iterate
    = Range Int Int
    | Values [String]
    | Bloks
    | Feeds
    | FeedGroups (Maybe Posts.Query) String
    | FeedPosts (Maybe Posts.Query)
    deriving (Read)


data SortOrder
    = None
    | Ascending
    | Descending
    | Shuffle Bool
    deriving (Eq, Read)


registerX ctxproj xreg =
    let

    renderer (maybectxpage , argstr) =
        if waitforpage
            then Nothing
            else Just$ (cfg-:prefix) ++ allcontents ++ (cfg-:suffix)
        where
        allcontents = Util.join (cfg-:joinwith) (iteratees >~ foreach)
        foreach (i,v) =
            Util.replaceSubs ["[:i:]" =: show i , "[:v:]" =: v] (cfg-:content)
        iteratees = Util.indexed (droptake (args-:skip) (args-:limit) (iter $args-:over)) where
            iter (Values values) =
                ordered$ values >~ wrapped
            iter (Range from to) =
                ordered$ [from..to] >~ (show~.wrapped)
            iter Bloks =
                ordered$ projbloknames >~ wrapped
            iter Feeds =
                ordered$ (projfeeds ++ projbloknames)
                            >~ wrapped
            iter (FeedGroups maybequery fieldname) =
                maybefieldfunc~>((Posts.feedGroups ctxbuild projposts projbloks maybequery) =|- [])
                    ~> (feedord $args-:order) >~ wrapped
                where
                maybefieldfunc =
                    Data.List.lookup fieldname (Posts.wellKnownFields True)
            iter (FeedPosts maybequery) =
                (Posts.feedPosts ctxbuild projposts projbloks (maybequery))
                    ~> (feedord $args-:order) >~ (fields2pairs ~. show ~. wrapped)
                where
                fields2pairs post =
                    (Posts.wellKnownFields False) >~ (Util.both (id =: (post-:)))
            feedord Ascending = reverse
            feedord (Shuffle perpage) = shuffle perpage
            feedord _ = id
        ctxbuild = case maybectxpage of
                    Nothing -> Posts.NoContext
                    Just ctxpage -> Posts.BuildContext
                                        (ctxpage-:Tmpl.lookupCachedPageRender) (ctxpage-:Tmpl.allPagesFiles)
                                                            projbloks projposts (ctxproj-:Proj.setup-:Proj.cfg)
        wrapped = case args-:wrap of
                    Just (w1,w2) -> (w1++).(++w2)
                    Nothing      -> id
        droptake 0 0 = id
        droptake 0 t = (take t)
        droptake d 0 = (drop d)
        droptake d t = (drop d) ~. (take t)
        ordered = case args-:order of
                    Ascending       -> Data.List.sort
                    Descending      -> Data.List.sortBy (flip compare)
                    Shuffle perpage  -> shuffle perpage
                    _               -> id
        args = X.tryParseArgs argstr (Just defargs) errargs where
            defargs = Args { over = Values [], wrap = Nothing, order = None, skip = 0, limit = 0 }
            errargs = Args { over = Values [X.htmlErr$ X.clarifyParseArgsError (xreg , (Util.excerpt 23 argstr))], wrap = Nothing, order = None, skip = 0, limit = 0 }

        shuffle perpage = Util.shuffleExtra (randseeds maybectxpage perpage)
        randseeds (Just pagectx) True =
            (pagectx-:Tmpl.randSeed) ++ (randseeds Nothing False)
        randseeds _ _ =
            ctxproj-:Proj.setup-:Proj.randSeed

        waitforpage =
            ((needpage4ord $args-:order) || (needpage4iter $args-:over))
                && (not$ hasctxpage maybectxpage)
            where
            hasctxpage Nothing = False ; hasctxpage (Just _) = True
            needpage4ord (Shuffle b) = b ; needpage4ord _ = False
            needpage4iter (FeedGroups q _) = needpage4query q
            needpage4iter (FeedPosts q) = needpage4query q
            needpage4iter _ = False
            needpage4query (Just (Posts.Filter feednames@(_:_) _ _)) =
                is projbloknames && any (`elem` projbloknames) feednames
            needpage4query _ =
                is projbloknames

    in X.EarlyOrWait renderer
    where

    projfeeds = ctxproj-:Proj.setup-:Proj.feeds
    projposts = ctxproj-:Proj.setup-:Proj.posts
    projbloks = ctxproj-:Proj.setup-:Proj.bloks
    projbloknames = Data.Map.Strict.keys projbloks
    cfg_parsestr = Tmpl.fixParseStr "content" (xreg-:X.cfgFullStr)
    cfg = X.tryParseCfg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { prefix = "" , suffix = "" , joinwith = "" , content = "" }
        errcfg = Cfg { prefix = X.htmlErr (X.clarifyParseCfgError xreg) , suffix = "" , joinwith = "" , content = "" }
