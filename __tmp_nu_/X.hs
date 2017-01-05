{-# OPTIONS_GHC -Wall #-}
module X where

import Base
import qualified Html
import qualified Util

import qualified Data.List
import qualified Data.Map.Strict



data Reg =
    Named {
        xname :: String,
        tname :: String,
        cfgFullStr :: String,
        cfgSplitAll :: [String],
        cfgSplitOnce ::  (String,String)
    }




_htmlattsfor xreg clarify codemain codemore =
    let (xn,tn) = (xreg~:xname , xreg~:tname) in
    [ "style" =: "background-color: yellow !important; color: red !important; border: solid 0.5em red !important; display: inline-block !important;"
    , "" =: "{!X| Bad syntax "++clarify++" `"++codemain++": "++ (Html.escape [] codemore)++"` (couldn't parse it) |!}"
    ]

htmlAttsForArgsParseError xreg arghint =
    let (xn,tn) = (xreg~:xname , xreg~:tname)
    in _htmlattsfor xreg ( "(for `" ++ xn ++"` args) within" ) ( "{<!---->X|" ++ tn ) arghint

htmlAttsForCfgParseError xreg =
    let (xn,tn) = (xreg~:xname , xreg~:tname)
        hint = Util.ifIs (Util.atOr (xreg~:cfgSplitAll) 0 "") (++": ...")
    in _htmlattsfor xreg ( "(in your *.haxproj) following" ) ( "X|:" ++ xn ++ ":" ++ tn ) (Util.excerpt 23 hint)




parseProjLines linessplits xregisterers =
    Data.Map.Strict.fromList$ linessplits>~foreach ~|fst~.is
    where
    foreach ("|X|":xname:tname:tvals) =
        let xn = Util.trim xname
            tn = Util.trim tname
        in if null tn then ( "" , id ) else
            case Data.List.lookup xn xregisterers of
                Nothing -> ( tn , rendererr ("{!X| Specified X-renderer `"++xname++"` not known |!}") )
                Just regx -> from regx xn tn tvals
    foreach _ =
        ( "" , id )
    from registerx xn tn tvals =
        let cfgstr = Util.trim$ Util.join ":" tvals
        in ( tn , registerx Named { xname = xn,
                                    tname = tn,
                                    cfgFullStr = cfgstr,
                                    cfgSplitAll = tvals>~Util.trim,
                                    cfgSplitOnce = Util.both' Util.trim (Util.splitOn1st ':' cfgstr) } )
    rendererr msg = \_ _ -> msg -- same as `rendererr = const.const` but why befuddle!




tagResolver xtags tagcontent =
    let
        (key , argstr) = Util.splitOn1st ':' tagcontent
        maybetag = Data.Map.Strict.lookup key xtags
    in case maybetag of
        Nothing -> Nothing
        Just xtag -> Just (xtag undefined (Util.trim argstr))
