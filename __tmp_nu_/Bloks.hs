{-# OPTIONS_GHC -Wall #-}

module Bloks where

import qualified Util
import Util ( (#) , (~>) , (>~) , (~|) )

import qualified Data.List
import qualified Data.Map.Strict
import qualified Data.Maybe
import qualified Text.Read


data Blok = NoBlok | Blok {
    title :: String,
    desc :: String,
    atomFile :: String,
    inSitemap :: Bool,
    dater :: String
} deriving (Eq, Read, Show)


_joinc = Util.join ":"


parseDefs linessplits =
    Data.Map.Strict.fromList$
    linessplits>~persplit ~|(/=noblok) where
        persplit ("B":"":bname:bvalsplits) =
            let parsestr = bvalsplits ~> _joinc ~> Util.trim ~> toParseStr
                parsed = (Text.Read.readMaybe parsestr) :: Maybe Blok
                errblok = Blok { title="{!syntax issue near `B::"++bname++":`, couldn't parse `"++parsestr++"`!}",
                                    desc="{!Syntax issue in your .haxproj file defining Blok named '"++bname++"'. Thusly couldn't parse Blok settings (incl. title/desc)!}",
                                    atomFile="", inSitemap=False, dater="" }
            in (bname , Data.Maybe.fromMaybe errblok parsed)
        persplit _ =
            noblok
        noblok = ("",NoBlok)


bTagResolver curbname hashmap str =
    let splits = Util.splitBy ':' str
        fields = [("title",title),("desc",desc),("atomFile",atomFile),("dater",dater)]
        fname = splits#0
        bname = Util.atOr splits 1 curbname
        blok = if null bname then NoBlok else
                Data.Map.Strict.findWithDefault NoBlok bname hashmap
        restore = "{B{"++(_joinc splits)++"}}"
    in if null splits then restore else
        if fname=="name" && Util.is bname
            then bname else if blok==NoBlok || null fname
                then restore else
                    case Data.List.lookup fname fields of
                        Just fieldval -> fieldval blok
                        Nothing -> restore


toParseStr projline =
    let
        pl = projline ~> (checkfield "title" "") ~> (checkfield "desc" "") ~>
                (checkfield "atomFile" "") ~> (checkfield "inSitemap" True) ~> (checkfield "dater" "")
    in
        "Blok {"++pl++"}" where
            checkfield field defval prjln =
                let haswith = (Util.contains prjln) . (field++)
                in if any haswith$
                    ["=\"", " = \"", "= \"", " =\"", "\t=\t\"", "=\t\"", "\t=\""]++
                    ["={", " = {", "= {", " ={", "\t=\t{", "=\t{", "\t={"]++
                    ["=True","=False"]
                    then prjln -- there was a hint field is already in def-string
                    else prjln++", "++field++"="++(show defval) -- user skipped field, append
