{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X.Repeat where

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
        wrap :: (String , String),
        order :: SortOrder,
        skip :: Int,
        limit :: Int
    }
    deriving Read


data Iterate
    = Range Int Int
    | Values [String]
    | Bloks
    | Feeds
    | FeedGroups (Maybe Posts.Query) String
    | FeedPosts (Maybe Posts.Query)
    deriving Read


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
                ordered$ (projfeednames ++ projbloknames)
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
        wrapped = args-:wrap ~> \(w1,w2) -> (w1++).(++w2)
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
            defargs = Args { over = Values [], wrap = ("",""), order = None, skip = 0, limit = 0 }
            errargs = Args { over = Values [X.htmlErr$ X.clarifyParseArgsError (xreg , (Util.excerpt 23 argstr))], wrap = ("",""), order = None, skip = 0, limit = 0 }

        shuffle perpage = Util.shuffleExtra (randseeds maybectxpage perpage)
        randseeds (Just pagectx) True =
            (pagectx-:Tmpl.randSeed) ++ (randseeds Nothing False)
        randseeds _ _ =
            ctxproj-:Proj.setup-:Proj.randSeed

        waitforpage =
            (not$ hasctxpage maybectxpage)
                && ( (needpage4iter $args-:over) || (needpage4ord $args-:order) )
            where
            hasctxpage Nothing = False ; hasctxpage _ = True
            needpage4ord (Shuffle perpage) = perpage ; needpage4ord _ = False
            needpage4iter (FeedGroups query _) = needpage4feed query
            needpage4iter (FeedPosts query) = needpage4feed query
            needpage4iter _ = False
            needpage4feed (Just (Posts.Filter feednames@(_:_) _ _)) =
                not$ any (`elem` projfeednames) feednames -- not known yet (or other placeholder), so postpone til page
            needpage4feed _ =
                is projbloknames
            _likelyinsnippet =
                let i1 = Util.indexOfSub argstr "[|" ; i2 = Util.indexOfSub argstr "|]"
                in i1 >= 0 && i2 > i1

    in X.EarlyOrWait renderer
    where

    projfeednames = ctxproj-:Proj.setup-:Proj.feeds
    projposts = ctxproj-:Proj.setup-:Proj.posts
    projbloks = ctxproj-:Proj.setup-:Proj.bloks
    projbloknames = Data.Map.Strict.keys projbloks
    cfg_parsestr = Tmpl.fixParseStr "content" (xreg-:X.cfgFullStr)
    cfg = X.tryParseCfg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { prefix = "" , suffix = "" , joinwith = "" , content = "" }
        errcfg = Cfg { prefix = X.htmlErr (X.clarifyParseCfgError xreg) , suffix = "" , joinwith = "" , content = "" }
