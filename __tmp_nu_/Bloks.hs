{-# OPTIONS_GHC -Wall #-}

module Bloks where

import qualified Util
import Util ( (~>) , (>~) )

import qualified Data.Map.Strict
import qualified Text.Read


data Blok = NoBlok | Blok {
    title :: String,
    desc :: String,
    atomFile :: String,
    dater :: String
} deriving (Eq, Read, Show)


parseDefs linessplits =
    Data.Map.Strict.fromList$
    (linessplits >~ bpersplit) ~> filter (/=noblok) where
        noblok = ("",NoBlok)
        bpersplit ("B":"":bname:bvalsplits) =
            let parsestr = bvalsplits ~> Util.join ":" ~> Util.trim ~> toParseString
            in case (Text.Read.readMaybe parsestr) :: Maybe Blok of
                Just blok -> (bname,blok)
                Nothing -> (bname, Blok { title="{!syntax issue near `B::"++bname++":`, couldn't parse `"++parsestr++"`!}",
                                        desc="{!Syntax issue in your .haxproj file defining Blok named '"++bname++"'. Thusly couldn't parse Blok settings (incl. title/desc)!}",
                                        atomFile="", dater="" })
        bpersplit _ =
            noblok


bTagResolver curbname bloks str =
    bvalue (Util.splitBy ':' str) bloks
    where
        bvalue ("title":bname:[]) hashmap =
            bvalhelper "title" bname title hashmap
        bvalue ("desc":bname:[]) hashmap =
            bvalhelper "desc" bname desc hashmap
        bvalue ("atomFile":bname:[]) hashmap =
            bvalhelper "atomFile" bname atomFile hashmap
        bvalue ("dater":bname:[]) hashmap =
            bvalhelper "dater" bname atomFile hashmap
        bvalue splits _ = -- reconstruct for later accesses
            let ifname = if "name"/=(Util.atOr splits 0 "") then
                            "" else Util.atOr splits 1 curbname
            in if null ifname then
                "{B{"++(Util.join ":" splits)++"}}" else ifname
        bvalhelper fname bname field hashmap =
            let name = if null bname then curbname else bname
                blok = Data.Map.Strict.findWithDefault NoBlok name hashmap
            in if blok==NoBlok then "{B{"++fname++":"++name++"}}" else field blok


toParseString projline =
    let
        pl = projline ~> (checkfield "title") ~> (checkfield "desc") ~> (checkfield "atomFile") ~> (checkfield "dater")
    in
        "Blok {"++pl++"}" where
            checkfield field prjln =
                let haswith = (Util.contains prjln) . (field++)
                in if any haswith (["=\"", " = \"", "= \"", " =\"", "\t=\t\"", "=\t\"", "\t=\""]++
                                    ["={", " = {", "= {", " ={", "\t=\t{", "=\t{", "\t={"]) then
                                        prjln else prjln++", "++field++"=\"\""
