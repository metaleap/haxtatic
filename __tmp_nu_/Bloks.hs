{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Bloks where

import Base
import qualified Defaults
import qualified Files
import qualified ProjC
import qualified Util

import qualified Data.List
import qualified Data.Map.Strict
import qualified System.FilePath



data Blok
    = B {
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
    in (sortedmatches ,  (null sortedmatches) |? Util.dateTime0 |! pagedate $sortedmatches~@0 )


blokByName bloks blokname =
    Data.Map.Strict.lookup blokname bloks


blokNameFromIndexPagePath possiblefakepath =
    -- possiblefakepath possibly sth like :B|/2016-12-18.tags
    (not$ Data.List.isPrefixOf Defaults.blokIndexPrefix possiblefakepath)
        |? "" |! drop 1 (System.FilePath.takeExtension possiblefakepath) -- cringe


blokNameFromRelPath bloks relpath file =
    Util.atOr (bloks~>Data.Map.Strict.keys >~ foreach ~|is) 0 ""
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
                bpage = Util.atOr allblokpagefiles 0 ("" , Files.NoFile)
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
        parsestr = projchunkval -- ~> (checkfield "title" "") ~> (checkfield "desc" "") ~>
                -- (checkfield "atomFile" "") ~> (checkfield "blokIndexPageFile" (bname++ ".html")) ~>
                --     (checkfield "inSitemap" True) ~> (checkfield "dtFormat" "")
    in "B {" ++parsestr++ "}"
    -- where
    -- checkfield field defval prjchnk =
    --     any ((Util.contains prjchnk).(field++)) (["=True" , "=False" , "={``:" , "=\""])
    --     |? prjchnk -- there was a hint field is already in def-string
    --     |! prjchnk ++ ", " ++ field ++ "=" ++ (show defval) -- user skipped field, append
