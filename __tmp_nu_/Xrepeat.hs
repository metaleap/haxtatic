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
        order :: SortOrder
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
    deriving (Eq, Read)


registerX ctxproj xreg =
    let
    renderer (Just pagectx , argstr) =
        Just$ (cfg-:prefix) ++ allcontents ++ (cfg-:suffix)
        where
        allcontents = Util.join (cfg-:joinwith) (iteratees >~ foreach)
        foreach (i,v) =
            Util.replaceSubs ["[:i:]" =: show i , "[:v:]" =: v] (cfg-:content)
        iteratees = Util.indexed$ (iter $args-:over) where
            iter (Values values) =
                ordered$ values >~ wrapped
            iter (Range from to) =
                ordered$ [from..to] >~ (show~.wrapped)
            iter Bloks =
                ordered$ (Data.Map.Strict.keys projbloks) >~ wrapped
            iter Feeds =
                ordered$ (projfeeds ++ (Data.Map.Strict.keys projbloks))
                            >~ wrapped
            iter (FeedGroups maybequery fieldname) =
                maybefieldfunc~>((Posts.feedGroups ctxbuild projposts projbloks maybequery) =|- [])
                    ~> (ord $args-:order) >~ wrapped
                where
                maybefieldfunc =
                    Data.List.lookup fieldname (Posts.wellKnownFields True)
            iter (FeedPosts maybequery) =
                (Posts.feedPosts ctxbuild projposts projbloks (maybequery))
                    ~> (ord $args-:order) >~ (fields2pairs ~. show ~. wrapped)
                where
                fields2pairs post =
                    (Posts.wellKnownFields False) >~ (Util.both (id =: (post-:)))
            ord Ascending = reverse ; ord _ = id
        ctxbuild = Posts.BuildContext (pagectx-:Tmpl.lookupCachedPageRender) (pagectx-:Tmpl.allPagesFiles)
                                                        projbloks projposts (ctxproj-:Proj.setup-:Proj.cfg)
        wrapped = case args-:wrap of
                    Just (w1,w2) -> (w1++).(++w2)
                    Nothing      -> id
        ordered = case args-:order of
                    Ascending   -> Data.List.sort
                    Descending  -> Data.List.sortBy (flip compare)
                    _           -> id
        args = X.tryParseArgs argstr (Just defargs) errargs where
            defargs = Args { over = Values [], wrap = Nothing, order = None }
            errargs = Args { over = Values [X.htmlErr$ X.clarifyParseArgsError (xreg , (Util.excerpt 23 argstr))], wrap = Nothing, order = None }

    renderer _ =
        Nothing

    in X.WaitForPage renderer
    where

    projfeeds = ctxproj-:Proj.setup-:Proj.feeds
    projposts = ctxproj-:Proj.setup-:Proj.posts
    projbloks = ctxproj-:Proj.setup-:Proj.bloks
    cfg_parsestr = Tmpl.fixParseStr "content" (xreg-:X.cfgFullStr)
    cfg = X.tryParseCfg cfg_parsestr (Just defcfg) errcfg where
        defcfg = Cfg { prefix = "" , suffix = "" , joinwith = "" , content = "" }
        errcfg = Cfg { prefix = X.htmlErr (X.clarifyParseCfgError xreg) , suffix = "" , joinwith = "" , content = "" }
