{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Bloks where

import Base
import qualified Defaults
import qualified Files
import qualified ProjC
import qualified Tmpl
import qualified Util

import qualified Data.List
import qualified Data.Map.Strict
import qualified System.FilePath



data Blok
    = B {
        title :: String,
        atomFile :: FilePath,
        blokIndexPageFile :: FilePath,
        inSitemap :: Bool,
        dtFormat :: String,
        desc :: String
    }
    deriving (Eq, Read)



allBlokPageFiles projcfg allpagesfiles bname =
    let blokpagematches = allpagesfiles~|isblokpage
        isblokpage (relpath,_) = isRelPathBlokPage bname relpath
        cmpblogpages file1 file2 =
            compare (pagedate file2) (pagedate file1)
        pagedate = snd.(Files.customDateFromFileName$ ProjC.dtPageDateParse projcfg)
        sortedmatches = Data.List.sortBy cmpblogpages blokpagematches
    in (sortedmatches , sortedmatches@?0 ~> (pagedate =|- Util.dateTime0))


blokByName bloks blokname =
    Data.Map.Strict.lookup blokname bloks


blokNameFromIndexPagePath possiblefakepath =
    -- possiblefakepath possibly sth like :B|/2016-12-18.tags
    (not$ Data.List.isPrefixOf Defaults.blokIndexPrefix possiblefakepath)
        |? "" |! drop 1 (System.FilePath.takeExtension possiblefakepath) -- cringe


blokNameFromRelPath bloks relpath file =
    "" -|= (bloks~>Data.Map.Strict.keys >~ foreach ~|is)@?0 -- yes, is
    where
    foreach bname
        | isRelPathBlokPage bname relpath
        = bname
        | otherwise
        = blokNameFromIndexPagePath $file-:Files.path



buildPlan (modtimeproj,modtimetmpl) projcfg allpagesfiles bloks =
    (dynpages , dynatoms) where
        dynatoms = mapandfilter (tofileinfo atomFile modtimeproj)
        dynpages = mapandfilter (tofileinfo blokIndexPageFile modtimetmpl)
        mapandfilter fn = isblokpagefile |~ (Data.Map.Strict.elems$ Data.Map.Strict.mapWithKey fn bloks)
        isblokpagefile (relpath,file) = is relpath && file /= Files.NoFile
        _allblokpagefiles = allBlokPageFiles projcfg allpagesfiles
        tofileinfo bfield modtime bname blok =
            let fakepath = (isblokpagefile bpage) |? blok-:bfield |! ""
                (allblokpagefiles , cdatelatest) = _allblokpagefiles bname
                bpage = ("",Files.NoFile) -|= allblokpagefiles@?0
                fdatelatest = maximum (allblokpagefiles >~ snd >~ Files.modTime)
            in ( Files.pathSepSlashToSystem fakepath ,
                (null fakepath) |? Files.NoFile |! Files.FileInfo {
                                                    Files.path =
                                                        Defaults.blokIndexPrefix++"/"
                                                        ++(ProjC.dtPageDateFormat projcfg cdatelatest)
                                                        ++"."++bname,
                                                    Files.modTime = max fdatelatest modtime } )



isRelPathBlokPage bname relpath =
    let patterns = [ bname++ ".*" , bname++(System.FilePath.pathSeparator:"*") ]
    in Files.simpleFilePathMatchAny relpath patterns



parseProjChunks chunkssplits =
    Data.Map.Strict.fromList$ chunkssplits >~ foreach ~> Util.unMaybes
    where
    foreach (blokname:bvalsplits) =
        case maybeblok of
            Nothing -> Nothing
            Just blok -> Just (bname , blok)
        where
        maybeblok = Util.tryParse Nothing (Just errblok) id ("Just "++parsestr)
        bname = blokname~>Util.trim
        parsestr = bvalsplits ~> (Util.join ":") ~> Util.trim ~> (toParseStr bname)
        errblok = B { title="{!|B| syntax issue near `|B|" ++ bname ++ ":`, couldn't parse `" ++ parsestr ++ "` |!}",
                            desc="{!|B| Syntax issue in your .haxproj file defining Blok named '" ++ bname ++
                                    "'. Thusly couldn't parse Blok settings (incl. title/desc) |!}",
                            atomFile="", blokIndexPageFile="", inSitemap=False, dtFormat="" }
    foreach _ =
        Nothing



tagHandler bloks curbname str =
    let fields = [  ("title",title) , ("desc",desc) , ("atomFile" , atomFile~.Files.pathSepSystemToSlash),
                    ("blokIndexPageFile" , blokIndexPageFile~.Files.pathSepSystemToSlash) , ("dtFormat",dtFormat)  ]
        bname = Util.ifNo bn curbname
        maybeblok = blokByName bloks bname
        (fname, bn) = Util.bothTrim (Util.splitOn1st ':' str)
    in (null fname) |? Nothing
        |! (fname=="name" && is bname) |? Just bname
            |! case maybeblok of
                Nothing -> Nothing
                Just blok -> case Data.List.lookup fname fields of
                                Just fieldval-> Just $blok-:fieldval
                                Nothing-> Nothing



toParseStr _bname projchunkval =
    let
        parsestr = Tmpl.fixParseStr "desc" projchunkval
    in "B {" ++ parsestr ++ "}"
