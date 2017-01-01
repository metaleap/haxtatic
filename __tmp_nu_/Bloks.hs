{-# OPTIONS_GHC -Wall #-}
module Bloks where

import qualified Defaults
import qualified Files
import qualified Util
import Util ( noNull , (#) , (~:) , (>~) , (~|) , (|~) , (~.) )

import qualified Data.List
import qualified Data.Map.Strict
import qualified Data.Maybe
import qualified System.FilePath
import qualified Text.Read


data Blok
    = NoBlok
    | Blok {
        title :: String,
        desc :: String,
        atomFile :: FilePath,
        blokIndexPageFile :: FilePath,
        inSitemap :: Bool,
        dater :: String
    }
    deriving (Eq, Read)


_joinc = Util.join ":"



allBlokPageFiles allpagesfiles bname =
    let blokpagematches = allpagesfiles~|isblokpage
        isblokpage (relpath,_) = isRelPathBlokPage bname relpath
        cmpblogpages (_,file1) (_,file2) =
            compare (Files.modTime file2) (Files.modTime file1)
    in Data.List.sortBy cmpblogpages blokpagematches



blokNameFromIndexPagePath possiblefakepath =
    let lenprefix = Defaults.blokIndexPrefix~:length
    in if Defaults.blokIndexPrefix == possiblefakepath~:(take lenprefix)
        then possiblefakepath~:(drop lenprefix)
        else ""



blokNameFromRelPath bloks relpath file =
    Util.atOr (bloks~:Data.Map.Strict.keys >~ foreach ~|noNull) 0 "" where
        foreach bname
            |(isRelPathBlokPage bname relpath) =
                bname
            |(otherwise)=
                blokNameFromIndexPagePath $file~:Files.path



bTagResolver hashmap curbname str =
    let splits = Util.splitBy ':' str
        fields = [  ("title",title) , ("desc",desc) , ("atomFile" , atomFile~.Files.pathSepSystemToSlash),
                    ("blokIndexPageFile" , blokIndexPageFile~.Files.pathSepSystemToSlash) , ("dater",dater)  ]
        fname = splits#0
        bname = if null bn then curbname else bn where
                    bn = Util.atOr splits 1 curbname
        blok = if null bname then NoBlok else
                Data.Map.Strict.findWithDefault NoBlok bname hashmap
        restore = if null curbname
                    then "{B{"++(_joinc splits)++"}}"
                    else "WTF:"++curbname++"-"++bname++"WTF"
    in if null splits then restore else
        if fname=="name" && bname~:noNull
            then bname else if blok==NoBlok || null fname
                then restore else
                    case Data.List.lookup fname fields of
                        Just fieldval-> fieldval blok
                        Nothing-> restore



buildPlan (modtimeproj,modtimetmplblok) allpagesfiles bloks =
    (dynpages , dynatoms) where
        dynatoms = mapandfilter (tofileinfo atomFile modtimeproj)
        dynpages = mapandfilter (tofileinfo blokIndexPageFile modtimetmplblok)
        mapandfilter fn = isblokpagefile |~ (Data.Map.Strict.elems$ Data.Map.Strict.mapWithKey fn bloks)
        isblokpagefile (relpath,file) = relpath~:noNull && file /= Files.NoFile
        tofileinfo bfield modtime bname blok =
            let virtpath = if isblokpagefile bpage then blok~:bfield else ""
                bpage@(_,bpagefile) = Util.atOr (allBlokPageFiles allpagesfiles bname) 0 ("" , Files.NoFile)
            in ( Files.pathSepSlashToSystem virtpath ,
                    if null virtpath then Files.NoFile else
                        Files.FileInfo (Defaults.blokIndexPrefix++bname) (max (Files.modTime bpagefile) modtime) )



isRelPathBlokPage bname relpath =
    let patterns = [ bname++".*" , bname++(System.FilePath.pathSeparator:"*") ]
    in Files.simpleFilePathMatchAny relpath patterns



parseDefs linessplits =
    Data.Map.Strict.fromList$
    linessplits>~foreach ~|(/=noblok) where
        foreach ("B":"":blokname:bvalsplits) =
            let bname = blokname~:Util.trim
                parsestr = bvalsplits ~: _joinc ~: Util.trim ~: (toParseStr bname)
                parsed = (Text.Read.readMaybe parsestr) :: Maybe Blok
                errblok = Blok { title="{!syntax issue near `B::"++bname++":`, couldn't parse `"++parsestr++"`!}",
                                    desc="{!Syntax issue in your .haxproj file defining Blok named '"++bname++"'. Thusly couldn't parse Blok settings (incl. title/desc)!}",
                                    atomFile="", blokIndexPageFile="", inSitemap=False, dater="" }
            in if null bname then noblok
                else (bname , Data.Maybe.fromMaybe errblok parsed)
        foreach _ =
            noblok
        noblok = ("" , NoBlok)



toParseStr bname projline =
    let
        pl = projline ~: (checkfield "title" "") ~: (checkfield "desc" "") ~:
                (checkfield "atomFile" "") ~: (checkfield "blokIndexPageFile" (bname++".html")) ~:
                    (checkfield "inSitemap" True) ~: (checkfield "dater" "")
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
