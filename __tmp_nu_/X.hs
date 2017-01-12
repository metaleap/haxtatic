{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module X where

import Base
import qualified Html
import qualified Util

import qualified Data.List
import qualified Data.Map.Strict


data Reg
    = Nope
    | Named {
        xname :: String,
        tname :: String,
        cfgFullStr :: String,
        cfgSplitAll :: [String],
        cfgSplitOnce ::  (String,String)
    } deriving (Show)


data Render r = NoRender | Early r | WaitForPage r



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
    "style" =: "background-color: yellow !important; color: red !important; border: solid 0.5em red !important; display: inline-block !important;"

htmlErrAttsCfg =
    htmlErrAtts . clarifyParseCfgError

htmlErrAttsArgs =
    htmlErrAtts . clarifyParseArgsError




parseProjChunks ctxproj xregisterers chunkssplits =
    Data.Map.Strict.fromList (chunkssplits>~foreach ~|fst~.is)
    where
    nope = ("" , NoRender)
    rendererr msg (_,_) = Just msg
    foreach (xname':tname':tvals) =
        let xn = Util.trim xname'
            tn = Util.trim tname'
        in if null tn then nope else
            case Data.List.lookup xn xregisterers of
                Nothing -> ( tn , Early (rendererr ("{!|X|"++tn++": unknown X-renderer `"++xn++"`, mispelled in your *.haxproj? |!}")) )
                Just regx -> from regx xn tn tvals
    foreach _ =
        nope
    from registerx xn tn tvals =
        let cfgstr = Util.trim$ Util.join ":" tvals
            xreg = registerx ctxproj Named {
                                    xname = xn,
                                    tname = tn,
                                    cfgFullStr = cfgstr,
                                    cfgSplitAll = tvals>~Util.trim,
                                    cfgSplitOnce = Util.bothTrim (Util.splitOn1st ':' cfgstr)
                                }
        in ( tn , xreg )



tagHandler xtags ctxpage tagcontent =
    renderwhen (Data.Map.Strict.lookup key xtags)
    where
    (key , argstr) = Util.splitOn1st ':' tagcontent
    renderargs = (ctxpage , Util.trim argstr)

    renderwhen (Just (Early xrend)) =
        xrend renderargs
    renderwhen (Just (WaitForPage xrend)) =
        case ctxpage of
            Nothing -> Just$ "{P|X|"++tagcontent++"|}"
            Just _ -> xrend renderargs
    renderwhen _ =
        Nothing



tryParseArgs parsestr maybedefargs errargs =
    let parse Nothing e = (Util.tryParseOr e) . wrap
        parse (Just d) e = Util.tryParse d e wrap
        wrap = (("Args{"++).(++"}"))
    in parse maybedefargs errargs parsestr

tryParseCfg parsestr maybedefcfg errcfg =
    let parse Nothing e = (Util.tryParseOr e) . wrap
        parse (Just d) e = Util.tryParse d e wrap
        wrap = (("Cfg{"++).(++"}"))
    in parse maybedefcfg errcfg parsestr
