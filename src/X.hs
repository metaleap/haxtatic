{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X where

import Base
import qualified Html
import qualified Util

import qualified Data.List
import qualified Data.Map.Strict


data Reg
    = Named {
        xname :: String,
        tname :: String,
        cfgFullStr :: String,
        cfgSplitAll :: [String],
        cfgSplitOnce ::  (String,String)
    }


data Render r = Early r | EarlyOrWait r | WaitForPage r



clarifyParseArgsError (xreg , arghint) =
    let (xn,tn) = (xreg-:xname , xreg-:tname)
    in ( ("(for `" ++ xn ++"` args) near") , ("{X<!---->|" ++ tn) , arghint )

clarifyParseCfgError xreg =
    let (xn,tn) = (xreg-:xname , xreg-:tname)
        hint = Util.ifIs ("" -|= (xreg-:cfgSplitAll)@?0) (++": ...")
    in ( ("(in your *.haxproj) following") , ("X|:" ++ xn ++ ":" ++ tn) , (Util.excerpt 23 hint) )

htmlErr (clarify , codemain , codemore) =
    "{!|X| Bad syntax "++clarify++" `"++codemain++": "++ (Html.escape codemore)++"` (couldn't parse it) |!}"

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




parseProjChunks ctxproj xregisterers chunkssplits =
    Data.Map.Strict.fromList (chunkssplits>~foreach ~> Util.unMaybes)
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
        reg = registerx ctxproj Named { xname = xn,
                                        tname = tn,
                                        cfgFullStr = cfgstr,
                                        cfgSplitAll = tvals>~Util.trim,
                                        cfgSplitOnce = Util.bothTrim (Util.splitOn1st ':' cfgstr) }
        cfgstr = Util.trim$ Util.join ":" tvals



tagHandler xtags ctxpage tagcontent =
    renderwhen$ Data.Map.Strict.lookup xtagname xtags
    where
    renderargs = (ctxpage , Util.trim argstr)
    (xtagname , argstr) = Util.splitOn1st ':' tagcontent

    renderwhen (Just (Early xrend)) =
        xrend renderargs
    renderwhen (Just (WaitForPage xrend)) =
        case ctxpage of
            Nothing -> Just$ "{P|X|"++tagcontent++"|}"
            Just _ -> xrend renderargs
    renderwhen (Just (EarlyOrWait xrend)) =
        Just$ ("{P|X|"++tagcontent++"|}") -|= (xrend renderargs)
    renderwhen _ =
        Nothing



tryParseArgs parsestr =
    _tryparse "Args" parsestr

tryParseCfg parsestr =
    _tryparse "Cfg" parsestr

_tryparse ctorname parsestr maybedefval errval =
    let wrap = (((ctorname++"{")++).(++"}"))
        try (Just defval) = Util.tryParse defval errval wrap
        try _ = (Util.tryParseOr errval) . wrap
    in try maybedefval parsestr
