{-# OPTIONS_GHC -Wall #-}

module Bloks where

import qualified Files
import qualified Util
import Util ( (#) , (~>) , (>~) , (~|) , (|~) , (~.) )

import qualified Data.List
import qualified Data.Map.Strict
import qualified Data.Maybe
import qualified System.FilePath
import qualified Text.Read


data Blok = NoBlok | Blok {
    title :: String,
    desc :: String,
    atomFile :: String,
    blokPageFile :: String,
    inSitemap :: Bool,
    dater :: String
} deriving (Eq, Read, Show)


_joinc = Util.join ":"


bTagResolver curbname hashmap str =
    let splits = Util.splitBy ':' str
        fields = [("title",title),("desc",desc),("atomFile",atomFile),("blokPageFile",blokPageFile),("dater",dater)]
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



allBlokPageFiles allpagesfiles bname =
    let cmpblogpages (_,file1) (_,file2) =
            compare (Files.modTime file2) (Files.modTime file1)
        blokpagematches = allpagesfiles ~|(\(relpath , _) -> isblokpage relpath)
        isblokpage = isRelPathBlokPage bname
    in Data.List.sortBy cmpblogpages blokpagematches



buildPlan ::
    [(String , Files.File)]->
    Data.Map.Strict.Map String Blok->
    ( [(String , Files.File)] , [(String , Files.File)] )
buildPlan [] _ = ([] , [])
buildPlan allpagesfiles bloks =
    (dynpages , dynatoms) where
        dynatoms = mapandfilter toatominfo
        dynpages = mapandfilter toatominfo
        mapandfilter fn = empties |~ (Data.Map.Strict.elems$ Data.Map.Strict.mapWithKey fn bloks)
        empties (relpath,file) = Util.is relpath && file /= Files.NoFile
        allblokpagefiles = allBlokPageFiles allpagesfiles
        latest bname =
            Util.atOr (allblokpagefiles bname) 0 ("" , Files.NoFile)
        toatominfo bname blok =
            let virtpath = if null bpagerelpath then "" else blok~>atomFile
                (bpagerelpath , bpagefile) = latest bname -- bpagerelpath is pages-dir-rel: foo\basics\intro.html
            in (virtpath , if null virtpath then Files.NoFile else bpagefile)



isRelPathBlokPage bname relpath =
    Files.simpleFilePathMatchAny relpath$
        [ bname++".*" , bname++(System.FilePath.pathSeparator:"*") ]



parseDefs linessplits =
    Data.Map.Strict.fromList$
    linessplits>~persplit ~|(/=noblok) where
        persplit ("B":"":bname:bvalsplits) =
            let parsestr = bvalsplits ~> _joinc ~> Util.trim ~> (toParseStr bname)
                parsed = (Text.Read.readMaybe parsestr) :: Maybe Blok
                errblok = Blok { title="{!syntax issue near `B::"++bname++":`, couldn't parse `"++parsestr++"`!}",
                                    desc="{!Syntax issue in your .haxproj file defining Blok named '"++bname++"'. Thusly couldn't parse Blok settings (incl. title/desc)!}",
                                    atomFile="", blokPageFile="", inSitemap=False, dater="" }
            in (bname , Data.Maybe.fromMaybe errblok parsed)
        persplit _ =
            noblok
        noblok = ("",NoBlok)



toParseStr bname projline =
    let
        pl = projline ~> (checkfield "title" "") ~> (checkfield "desc" "") ~>
                (checkfield "atomFile" "") ~> (checkfield "blokPageFile" (bname++".html")) ~> (checkfield "inSitemap" True) ~> (checkfield "dater" "")
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
