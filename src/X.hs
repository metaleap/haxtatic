module X where

import Base

import qualified Html
import qualified ProjC
import qualified Tmpl
import qualified Util

import qualified Data.List
import qualified Data.Map.Strict


data Reg
    = Named {
        xname :: String,
        tname :: String,
        cfgFullStr :: String,
        cfgSplitAll :: [String],
        cfgSplitOnce ::  (String,String),
        parsingFailEarly :: Bool
    }


data Render = Early Renderer | EarlyOrWait Renderer | WaitForPage Renderer
type Renderer = ((Maybe Tmpl.CtxPage , String) -> Maybe String)



clarifyParseArgsError (xreg , arghint) =
    let (xn,tn) = (xreg-:xname , xreg-:tname)
    in ( ("(for `" ++ xn ++"` args) near") , ("{X<!---->|" ++ tn ++ ":") , arghint )

clarifyParseCfgError xreg =
    let (xn,tn) = (xreg-:xname , xreg-:tname)
        hint = Util.ifIs ("" -|= (xreg-:cfgSplitAll)@?0) (++": ...")
    in ( ("(in your *.haxproj) following") , ("|X|" ++ xn ++ ":" ++ tn ++ ":") , (Util.excerpt 23 hint) )

hasPageContext Nothing = False
hasPageContext _ = True

hasNoPageContext Nothing = True
hasNoPageContext _ = False

htmlAttsNeedPage [] =
    False
htmlAttsNeedPage ((('/':_),_):_) =
    True
htmlAttsNeedPage (_:more) =
    htmlAttsNeedPage more


htmlErr (clarify , codemain , codemore) =
    "{!|X| Bad syntax "++clarify++" `<code>"++codemain++" "++ (Html.escape codemore)++"</code>` (couldn't parse it) |!}"

htmlErrAtts clarifywithcode =
    [ "" =: htmlErr clarifywithcode
    , htmlErrStyle
    ]

htmlErrStyle =
    "style" =: "background-color: yellow !important; color: red !important; border: solid 0.123em red !important; display: inline-block !important;"

htmlErrAttsCfg =
    htmlErrAtts . clarifyParseCfgError

htmlErrAttsArgs =
    htmlErrAtts . clarifyParseArgsError


htmlPathAtts _ [] =
    []
htmlPathAtts ctxpage ((('/':name),value):more) =
    (name , value -|= (ctxpage-:Tmpl.pTagHandler) ('/':value)) : (htmlPathAtts ctxpage more)
htmlPathAtts ctxpage (normal:more) =
    normal : (htmlPathAtts ctxpage more)


parseProjChunks ctxproj projcfg xregisterers chunkssplits =
    Data.Map.Strict.fromList (chunkssplits>=~foreach)
    where
    rendererr msg (_,_) = Just msg
    foreach (xname':tname':tvals) =
        let xn = Util.trim xname'
            tn = Util.trim tname'
        in if null tn then Nothing else
        Just$ case Data.List.lookup xn xregisterers of
            Nothing -> ( tn , Early (rendererr ("{!|X|"++tn++": unknown X-renderer `"++xn++"`, mispelled in your *.haxproj? |!}")) )
            Just regx -> from regx xn tn tvals
    foreach _ =
        Nothing
    from registerx xn tn tvals =
        ( tn , reg ) where
        reg = registerx ctxproj Named { xname = xn, tname = tn, parsingFailEarly = projcfg-:ProjC.parsingFailEarly,
                                        cfgFullStr = cfgstr, cfgSplitAll = tvals>~Util.trim,
                                        cfgSplitOnce = Util.both (Util.trimEnd , Util.trimStart) (Util.splitOn1st_ ':' cfgstr) }
        cfgstr = Util.trim$ Util.join ":" tvals


shouldWaitForPage (Just _) _ _ =
    False
shouldWaitForPage Nothing needctxpage [] =
    needctxpage
shouldWaitForPage Nothing needctxpage htmlattribs =
    needctxpage || htmlAttsNeedPage htmlattribs


tagHandler xtags ctxpage tagcontent =
    renderwhen$ Data.Map.Strict.lookup xtagname xtags
    where
    renderargs = (ctxpage , Util.trimStart argstr)
    (xtagname , argstr) = (Util.splitOn1st_ ':' tagcontent)

    renderwhen (Just (Early xrend)) =
        xrend renderargs
    renderwhen (Just (WaitForPage xrend)) =
        case ctxpage of
            Nothing -> Just$ "{P|X|"++tagcontent++"|}"
            Just _ -> xrend renderargs
    renderwhen (Just (EarlyOrWait xrend)) =
        --  xrend with ctxpage=Nothing *might* give Nothing if early-not-on, only then trigger delay-tag
        Just$ ("{P|X|"++tagcontent++"|}") -|= (xrend renderargs)
    renderwhen _ =
        Nothing



tryParseArgs xreg =
    _tryparse xreg "Args"

tryParseCfg xreg =
    _tryparse xreg "Cfg"

_tryparse xreg ctorname parsestr maybedefval errval =
    let wrap = (((ctorname++"{")++).(++"}"))
        err val |(xreg-:parsingFailEarly)= ProjC.raiseParseErr (ctorname=="Cfg" |? "(*.haxproj or *.haxsnip)" |! "(?)") (for ctorname) parsestr
                |(otherwise)= val
        for "Cfg" = "|X|"++(xreg-:xname)++":"++(xreg-:tname)++":.."
        for "Args" = "{X|"++(xreg-:tname)++": .. |}"
        for _ = undefined
        try (Just defval) = Util.tryParse defval (err errval) wrap
        try _ = (Util.tryParseOr (err errval)) . wrap
    in try maybedefval parsestr
