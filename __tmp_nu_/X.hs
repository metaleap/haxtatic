{-# OPTIONS_GHC -Wall #-}
module X where

import Base
import qualified Html
import qualified Tmpl
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
    }


data Render r = NoRender | Early r | WaitForPage r



_htmlattsfor xreg clarify codemain codemore =
    let (xn,tn) = (xreg.:xname , xreg.:tname) in
    [ "style" =: "background-color: yellow !important; color: red !important; border: solid 0.5em red !important; display: inline-block !important;"
    , "" =: "{!X| Bad syntax "++clarify++" `"++codemain++": "++ (Html.escape [] codemore)++"` (couldn't parse it) |!}"
    ]

htmlAttsForArgsParseError xreg arghint =
    let (xn,tn) = (xreg.:xname , xreg.:tname)
    in _htmlattsfor xreg ( "(for `" ++ xn ++"` args) within" ) ( "{<!---->X|" ++ tn ) arghint

htmlAttsForCfgParseError xreg =
    let (xn,tn) = (xreg.:xname , xreg.:tname)
        hint = Util.ifIs (Util.atOr (xreg.:cfgSplitAll) 0 "") (++": ...")
    in _htmlattsfor xreg ( "(in your *.haxproj) following" ) ( "X|:" ++ xn ++ ":" ++ tn ) (Util.excerpt 23 hint)




parseProjChunks xregisterers chunkssplits =
    Data.Map.Strict.fromList (chunkssplits>~foreach ~|fst~.is)
    where
    nope = ("" , NoRender)
    rendererr msg (_,_) = Just msg
    foreach (xname:tname:tvals) =
        let xn = Util.trim xname
            tn = Util.trim tname
        in if null tn then nope else
            case Data.List.lookup xn xregisterers of
                Nothing -> ( tn , Early (rendererr ("{!X|"++tn++": unknown X-renderer `"++xname++"`, mispelled in your *.haxproj? |!}")) )
                Just regx -> from regx xn tn tvals
    foreach _ =
        nope
    from registerx xn tn tvals =
        let cfgstr = Util.trim$ Util.join ":" tvals
            xreg = registerx Named {
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
