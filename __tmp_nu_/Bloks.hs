{-# OPTIONS_GHC -Wall #-}
module Bloks where

import qualified Defaults
import qualified Files
import qualified ProjC
import qualified Util
import Util ( is , (~:) , (>~) , (~|) , (|~) , (~.) , (|?) , (|!) , (#) )

import qualified Data.List
import qualified Data.Map.Strict
import qualified Data.Maybe
import qualified System.FilePath



data Blok
    = NoBlok
    | Blok {
        title :: String,
        desc :: String,
        atomFile :: FilePath,
        blokIndexPageFile :: FilePath,
        inSitemap :: Bool,
        dtFormat :: String
    }
    deriving (Eq, Read)



allBlokPageFiles projcfg allpagesfiles bname =
    let blokpagematches = allpagesfiles~|isblokpage
        isblokpage (relpath,_) = isRelPathBlokPage bname relpath
        cmpblogpages file1 file2 =
            compare (pagedate file2) (pagedate file1)
        pagedate = snd.(Files.customDateFromFileName$ ProjC.dtPageDateParse projcfg)
        sortedmatches = Data.List.sortBy cmpblogpages blokpagematches
    in (sortedmatches ,  (null sortedmatches) |? Util.dateTime0 |! pagedate $sortedmatches#0 )



blokNameFromIndexPagePath possiblefakepath =
    let lenprefix = Defaults.blokIndexPrefix~:length
    in Defaults.blokIndexPrefix /= possiblefakepath~:(take lenprefix)
        |? "" |! drop 1 (System.FilePath.takeExtension possiblefakepath)



blokNameFromRelPath bloks relpath file =
    Util.atOr (bloks~:Data.Map.Strict.keys >~ foreach ~|is) 0 "" where
        foreach bname
            | isRelPathBlokPage bname relpath
            = bname
            | otherwise
            = blokNameFromIndexPagePath $file~:Files.path



buildPlan (modtimeproj,modtimetmplblok) projcfg allpagesfiles bloks =
    (dynpages , dynatoms) where
        dynatoms = mapandfilter (tofileinfo False atomFile modtimeproj)
        dynpages = mapandfilter (tofileinfo True blokIndexPageFile modtimetmplblok)
        mapandfilter fn = isblokpagefile |~ (Data.Map.Strict.elems$ Data.Map.Strict.mapWithKey fn bloks)
        isblokpagefile (relpath,file) = is relpath && file /= Files.NoFile
        _allblokpagefiles = allBlokPageFiles projcfg allpagesfiles
        tofileinfo ispage bfield modtime bname blok =
            let virtpath = (isblokpagefile bpage) |? blok~:bfield |! ""
                (allblokpagefiles , datelatest) = _allblokpagefiles bname
                bpage@(_,bpagefile) = Util.atOr allblokpagefiles 0 ("" , Files.NoFile)
            in ( Files.pathSepSlashToSystem virtpath ,
                (null virtpath) |? Files.NoFile |! Files.FileInfo {
                                                    Files.path =
                                                        Defaults.blokIndexPrefix++"/"
                                                        ++(ProjC.dtPageDateFormat projcfg datelatest)
                                                        ++"."++bname,
                                                    Files.modTime = max (Files.modTime bpagefile) modtime } )



isRelPathBlokPage bname relpath =
    let patterns = [ bname++ ".*" , bname++(System.FilePath.pathSeparator:"*") ]
    in Files.simpleFilePathMatchAny relpath patterns



parseProjLines linessplits =
    Data.Map.Strict.fromList$
    linessplits>~foreach ~|(/=noblok) where
        noblok = ("" , NoBlok)
        foreach ("|B|":blokname:bvalsplits) =
            (bname , Util.tryParse NoBlok errblok parsestr)
            where
            bname = blokname~:Util.trim
            parsestr = bvalsplits ~: (Util.join ":") ~: Util.trim ~: (toParseStr bname)
            errblok = Blok { title="{!B| syntax issue near `B::" ++bname++ ":`, couldn't parse `" ++parsestr++ "` |!}",
                                desc="{!B| Syntax issue in your .haxproj file defining Blok named '" ++bname++
                                        "'. Thusly couldn't parse Blok settings (incl. title/desc) |!}",
                                atomFile="", blokIndexPageFile="", inSitemap=False, dtFormat="" }
        foreach _ =
            noblok



tagResolver hashmap curbname str =
    let (fname, bn) = Util.both' Util.trim (Util.splitAt1st ':' str)
        fields = [  ("title",title) , ("desc",desc) , ("atomFile" , atomFile~.Files.pathSepSystemToSlash),
                    ("blokIndexPageFile" , blokIndexPageFile~.Files.pathSepSystemToSlash) , ("dtFormat",dtFormat)  ]
        bname = (null bn) |? curbname |! bn
        blok = Data.Map.Strict.findWithDefault NoBlok bname hashmap
    in (null fname) |? Nothing
        |! (fname=="name" && is bname) |? Just bname
            |! (blok==NoBlok) |? Nothing
                |! case Data.List.lookup fname fields of
                    Just fieldval-> Just $blok~:fieldval
                    Nothing-> Nothing



toParseStr bname projline =
    let
        pl = projline ~: (checkfield "title" "") ~: (checkfield "desc" "") ~:
                (checkfield "atomFile" "") ~: (checkfield "blokIndexPageFile" (bname++ ".html")) ~:
                    (checkfield "inSitemap" True) ~: (checkfield "dtFormat" "")
    in "Blok {" ++pl++ "}"
    where
    checkfield field defval prjln =
        any ((Util.contains prjln).(field++)) ( ["=True","=False"]++
                ["={", " = {", "= {", " ={", "\t=\t{", "=\t{", "\t={"]++
                    ["=\"", " = \"", "= \"", " =\"", "\t=\t\"", "=\t\"", "\t=\""] )
        |? prjln -- there was a hint field is already in def-string
        |! prjln ++ ", " ++ field ++ "=" ++ (show defval) -- user skipped field, append
