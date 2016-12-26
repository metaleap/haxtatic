{-# OPTIONS_GHC -Wall #-}

module Bloks where

import qualified Util
import Util ( (~>) , (>~) )

import qualified Data.List
import qualified Data.Map.Strict
import qualified Text.Read


data Blok = NoBlok | Blok {
    title :: String,
    desc :: String,
    atomFile :: String,
    dater :: String
} deriving (Eq, Read, Show)


_joinc = Util.join ":"


parseDefs linessplits =
    Data.Map.Strict.fromList$
    (linessplits >~ bpersplit) ~> filter (/=noblok) where
        noblok = ("",NoBlok)
        bpersplit ("B":"":bname:bvalsplits) =
            let parsestr = bvalsplits ~> _joinc ~> Util.trim ~> toParseString
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
        bvalue splits hashmap =
            let fields = [("title",title),("desc",desc),("atomFile",atomFile),("dater",dater)]
                fname = Util.atOr splits 0 ""
                bname = Util.atOr splits 1 curbname
                blok = if null bname then NoBlok else Data.Map.Strict.findWithDefault NoBlok bname hashmap
                restore = "{B{"++(_joinc splits)++"}}"
            in if fname=="name" && (not.null) bname
                then bname else if blok==NoBlok || null fname
                    then restore else
                        case Data.List.lookup fname fields of
                            Just fieldval -> fieldval blok
                            Nothing -> restore


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
