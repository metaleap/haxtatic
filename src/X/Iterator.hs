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
        over :: Iteration,
        wrapEach :: (String , String),
        order :: SortOrder,
        skip :: Int,
        limit :: Int,
        more :: [String]
    }
    deriving Read


data Iteration
    = Range Int Int
    | Values [String]
    | BlokNames
    | FeedNames
    | FeedGroups (Maybe Posts.Query) String
    | FeedPosts (Maybe Posts.Query) String Util.StringPairs
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
        if waitforpage then Nothing
            else Just$ cfgwrap allcontents
        where
        allcontents = cfgjoin (iteratees >~ foreach)

        iteratees = Util.indexed (droptake (args-:skip) (args-:limit) (iter $args-:over)) where
            iter (Values values) =
                ordered$ values >~ wrapped
            iter (Range from to) =
                ordered$ [from..to] >~ (show~.wrapped)
            iter BlokNames =
                ordered$ projbloknames >~ wrapped
            iter FeedNames =
                ordered$ (projfeednames ++ projbloknames)
                            >~ wrapped
            iter (FeedGroups maybequery fieldname) =
                maybefieldfunc~>((Posts.feedGroups maybectxbuild projposts projbloks maybequery) =|- [])
                    ~> (feedord $args-:order) >~ wrapped
                where
                maybefieldfunc =
                    Data.List.lookup fieldname (Posts.wellKnownFields True)
            iter (FeedPosts maybequery dtformat morehtmls) =
                (Posts.feedPosts maybectxbuild projposts projbloks (maybequery) dtformat morehtmls)
                    ~> (feedord $args-:order) >~ (fields2pairs ~. show ~. (Util.crop 1 1) ~. wrapped)
                where
                fields2pairs post =
                    ((Posts.wellKnownFields False) ++ morefields) >~ (Util.both (id , (post-:)))
                morefields = (morehtmls>~Posts.moreFromHtmlFieldName ++ args-:more) >~ topair where
                    topair mfield =
                        mfield =: Posts.more~.(Util.lookup mfield $"{!|"++mfield++"|!}")
            feedord None = id
            feedord Descending = id
            feedord Ascending = reverse
            feedord (Shuffle perpage) = shuffle perpage
        maybectxbuild = maybectxpage =>- \ctxpage -> Posts.BuildContext (ctxpage-:Tmpl.lookupCachedPageRender)
                                                                        (ctxpage-:Tmpl.allPagesFiles) projbloks
                                                                        projposts (ctxproj-:Proj.setup-:Proj.cfg)
        wrapped = args-:wrapEach ~> \(w1,w2) -> (w1++).(++w2)
        droptake 0 0 = id
        droptake 0 t = (take t)
        droptake d 0 = (drop d)
        droptake d t = (drop d) ~. (take t)
        ordered = case args-:order of
                    Ascending       -> Data.List.sortBy compare
                    Descending      -> Data.List.sortBy (flip compare)
                    Shuffle perpage -> shuffle perpage
                    None            -> id
        args = X.tryParseArgs xreg argstr (Just defargs) errargs where
            defargs = Args { over = Values [], wrapEach = ("",""), order = None, skip = 0, limit = 0, more = [] }
            errargs = Args { over = Values [X.htmlErr$ X.clarifyParseArgsError (xreg , (Util.excerpt 23 argstr))], wrapEach = ("",""), order = None, skip = 0, limit = 0, more = [] }

        shuffle perpage = Util.shuffleExtra (randseeds maybectxpage perpage)
        randseeds (Just pagectx) True =
            (pagectx-:Tmpl.randSeed) ++ (randseeds Nothing False)
        randseeds _ _ =
            ctxproj-:Proj.setup-:Proj.randSeed

        waitforpage =
            (not$ hasctxpage maybectxpage)
                && ( (needpage4iter $args-:over) || (needpage4ord $args-:order) )
            where
            hasctxpage Nothing = False ; hasctxpage (Just _) = True
            needpage4ord (Shuffle perpage) = perpage ; needpage4ord _ = False
            needpage4iter (FeedGroups query _) = needpage4feed query
            needpage4iter (FeedPosts query _ _) = needpage4feed query
            needpage4iter _ = False
            needpage4feed (Just (Posts.Filter feednames@(_:_) _ _)) =
                not$ all (`elem` projfeednames) feednames -- not all known as |P| feeds (yet), so postpone til page
            needpage4feed _ =
                has projbloknames
            -- _likelyinsnippet =
            --     let i1 = Util.indexOfSub argstr "{%" ; i2 = Util.indexOfSub argstr "%}"
            --     in i1 >= 0 && i2 > i1

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
        defcfg = Cfg { prefix = "" , suffix = "" , joinVia = "" , content = "" }
        errcfg = Cfg { prefix = X.htmlErr (X.clarifyParseCfgError xreg) , suffix = "" , joinVia = "" , content = "" }




_repl   (False,False)   txt                         _               = txt
_repl   _               []                          _               = []
_repl   iv@(True,_)     ('{':'%':'i':'%':'}':rest)  both@(i , _)    = (show i) ++ _repl iv rest both
_repl   iv@(_,True)     ('{':'%':'v':'%':'}':rest)  both@(_ , v)    = v ++ _repl iv rest both
_repl   iv              (this : rest)               both            = this : _repl iv rest both

_hasi [] = False ; _hasi ('{':'%':'i':'%':'}':_) = True ; _hasi (_:rest) = _hasi rest
_hasv [] = False ; _hasv ('{':'%':'v':'%':'}':_) = True ; _hasv (_:rest) = _hasv rest
