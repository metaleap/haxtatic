module Bloks (  Blok, title, desc, inSitemap,
                allBlokPageFiles, blokByName, blokIndexPageFile, blokNameFromIndexPagePath,
                blokNameFromRelPath, buildPlan, tagHandler, parseProjChunks, preferredRelPath)
where

import HxB

import qualified Files
import qualified ProjC
import qualified Tmpl
import qualified Util

import qualified Data.List
import qualified Data.Map.Strict
import qualified System.FilePath



data Blok
    = From {
        title :: String,
        atomFile :: FilePath,
        blokIndexPageFile :: FilePath,
        inSitemap :: Bool,
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


blokByName _ "" =
    Nothing
blokByName bloks blokname =
    Data.Map.Strict.lookup blokname bloks


blokNameFromIndexPagePath possiblefakepath =
    -- possiblefakepath possibly sth like :B|/2016-12-18.tags
    (not$ Files.hasPathBlokIndexPrefix possiblefakepath)
        |? "" |! drop 1 (System.FilePath.takeExtension possiblefakepath) -- cringe


blokNameFromRelPath bloks relpath file =
    "" -|= (bloks~>Data.Map.Strict.keys >/~ foreach)@?0
    where
    foreach bname
        | isRelPathBlokPage bname relpath
        = bname
        | otherwise
        = blokNameFromIndexPagePath $file-:Files.path



buildPlan (modtimeproj,modtimetmpl) projcfg allpagesfiles bloks =
    (pagestogenerate , atomstogenerate) where
        atomstogenerate = files2gen (tofileinfo atomFile modtimeproj)
        pagestogenerate = files2gen (tofileinfo blokIndexPageFile modtimetmpl)
        files2gen blok2file = (Data.Map.Strict.elems (Data.Map.Strict.mapWithKey blok2file bloks))
                                ~| isblokpagefile
        isblokpagefile (relpath,file) = has relpath && file /= Files.NoFile
        _allblokpagefiles = allBlokPageFiles projcfg allpagesfiles
        tofileinfo bfield modtime bname blok =
            let fakepath = (isblokpagefile bpage) |? blok-:bfield |! ""
                (allblokpagefiles , cdatelatest) = _allblokpagefiles bname
                bpage = ("",Files.NoFile) -|= allblokpagefiles@?0
                fdatelatest = maximum (allblokpagefiles >~ (Files.modTime . snd))
            in ( Files.pathSepSlashToSystem fakepath ,
                (null fakepath) |? Files.NoFile |! Files.FileInfo {
                                                    Files.path = Files.prependPathBlokIndexPrefix$
                                                                    (ProjC.dtPageDateFormat projcfg cdatelatest)++('.' : bname),
                                                    Files.modTime = max fdatelatest modtime } )



isRelPathBlokPage bname relpath =
    let patterns = [ bname++ ".*" , bname++(System.FilePath.pathSeparator:"*") ]
    in Files.simpleFilePathMatchAny relpath patterns



parseProjChunks projcfg chunkssplits =
    Data.Map.Strict.fromList$ chunkssplits>~foreach
    where
    foreach (blokname:bvalsplits) =
        (bname , blok)
        where
        (bname , blok) = (blokname~>Util.trim , Util.tryParseOr (errblok) parsestr)
        parsestr' = bvalsplits ~> (Util.join ":") ~> Util.trim ~> (Tmpl.fixParseStr "desc")
        parsestr = parsestr' ~> (("From {"++).(++"}"))
        errblok = From { title = if projcfg-:ProjC.parsingFailEarly
                                    then (ProjC.raiseParseErr "*.haxproj" ("|B|"++bname++":") parsestr')
                                    else "{!|B| syntax issue near `|B|" ++ bname ++ ":`, couldn't parse `" ++ parsestr ++ "` |!}",
                            desc = "{!|B| Syntax issue in your .haxproj file defining Blok named '" ++ bname ++
                                    "'. Hence couldn't parse Blok settings (incl. title/desc) |!}",
                            atomFile = "", blokIndexPageFile = "", inSitemap = False }
    foreach _ =
        undefined



preferredRelPath bloks (outpagerelpath , file) =
    let bname = blokNameFromRelPath bloks outpagerelpath file
    in case blokByName bloks bname of
        Nothing -> outpagerelpath
        Just blok ->
            if (null $blok-:blokIndexPageFile) || (blok-:blokIndexPageFile) /= (bname++".html")
                then outpagerelpath
                else Util.substitute System.FilePath.pathSeparator '.' outpagerelpath



tagHandler bloks curbname str
    | (null fname) = Nothing
    | (fname=="name" && has bname) = Just bname
    | (otherwise) = (blokByName bloks bname) >>= \ blok ->
                        (Data.List.lookup fname fields) >~ \ fieldval ->
                            blok-:fieldval
    where
    (fname, bn) = Util.bothTrim (Util.splitOn1st_ ':' str)
    bname = Util.ifNo bn curbname
    fields = [  ("title",title), ("desc",desc),
                ("atomFile" , atomFile~.Files.pathSepSystemToSlash),
                ("blokIndexPageFile" , blokIndexPageFile~.Files.pathSepSystemToSlash)  ]
