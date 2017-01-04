{-# OPTIONS_GHC -Wall #-}
module Tmpl where

import qualified Defaults
import qualified Files
import qualified Util
import Util ( (#) , (~|) , (~:) , (>>~) , (>~) , (|?) , (|!) )

import qualified Data.List
import qualified Data.Maybe
import System.FilePath ( (</>) )



data Ctx
    = Processing {
        bTags :: String->String->Maybe String,
        cTags :: String->Maybe String,
        tTags :: String->Maybe String,
        xTags :: String->Maybe String,
        processTags :: [String]
    }
    | Template {
        fileExt :: String,
        srcFile :: Files.File,
        chunks :: [(String , String)]
    }



_applychunkbegin = "{P|:c"              --  we're really
_applychunkmid = "on"                   --  looking for:
_applychunkend = "tent:"++tag_Close     --  {P|:content:|}
apply ctxtmpl pagesrc =
    let foreach ("on","{P|:c") = pagesrc    --  OK we found one
        foreach (strange,"{P|:c") = _applychunkbegin++strange++_applychunkend -- the once-in-a-1000-years case .. someone went for {P|:cUriously persiStent:|} ?
        foreach (tmplsrc,_) = tmplsrc -- rest of template src as-is
    in concat$ ctxtmpl~:chunks>~foreach



loadAll ctxmain ctxproc deffiles filenameexts htmlequivexts =
    let foreach fileext
            | null fileext
            = loadTmpl ctxmain ctxproc "" $deffiles~:Defaults.htmlTemplateMain
            | fileext==Defaults.blokIndexPrefix
            =  loadTmpl ctxmain ctxproc Defaults.blokIndexPrefix $deffiles~:Defaults.htmlTemplateBlok
            | otherwise
            = let tmplpath name = "tmpl" </> (name $".haxtmpl"++fileext)
                in Files.readOrDefault False ctxmain (tmplpath ((ctxmain~:Files.setupName)++))
                    --  fallback path tmpl/default.haxtmpl.<fileext>
                    (tmplpath Defaults.fileName_Pref)
                        --  fallback template content: `{P|:content:|}`
                        (_applychunkbegin++_applychunkmid++_applychunkend)
                    >>= loadTmpl ctxmain ctxproc fileext
        fileexts = "":Defaults.blokIndexPrefix:otherexts where
            otherexts = Util.unique$ filenameexts ~| Util.noneOf htmlequivexts
    in fileexts>>~foreach
    >>= \loadedtemplates
    -> let
        tmpldef = loadedtemplates#0
        tmplfind "" = tmpldef
        tmplfind ".htm" = tmpldef
        tmplfind ".html" = tmpldef
        tmplfind ext =
            elem ext htmlequivexts |? tmpldef |!
                Data.Maybe.fromMaybe tmpldef $Data.List.find ((ext==).fileExt) loadedtemplates
    in return tmplfind



loadTmpl ctxmain ctxproc fileext tmpfile =
    warnIfTagMismatches ctxmain (srcfile~:Files.path) (tagMismatches rawsrc)
    >> return Template {
                fileExt = fileext, srcFile = srcfile, chunks = srcchunks
            }
    where
    srcfile = Files.fullFrom tmpfile Util.dateTime0 srcpreprocessed
    srcchunks = Util.splitUp [_applychunkbegin] _applychunkend srcpreprocessed
    srcpreprocessed = processSrcFully ctxproc "" rawsrc
    rawsrc = (tmpfile~:Files.content)



processSrcFully ctxproc =
    Util.repeatedly.(processSrcJustOnce ctxproc)


processSrcJustOnce ctxproc bname src =
    concat$ (Util.splitUp (ctxproc~:processTags) tag_Close src)>~foreach
    where
    foreach (srccontent , "") =
        srccontent
    foreach (tagcontent , tagbegin) =
        noesc |? result |! Util.crop 1 1 (show result)
        where
        noesc = not$ Util.startsWith tagcontent "``:"
        result = case (tagresolver taginner) of
            Just output-> output
            Nothing-> tagbegin++tagcontent++tag_Close
            where
            taginner = noesc |? tagcontent |! drop 3 tagcontent
            tagresolver
                |(tagbegin==tag_B)= (ctxproc~:bTags) bname
                |(tagbegin==tag_C)= ctxproc~:cTags
                |(tagbegin==tag_T)= ctxproc~:tTags
                |(tagbegin==tag_X)= ctxproc~:xTags
                |(otherwise)= const Nothing



tagMismatches src =
    (numtagbegins , numtagends)
    where
    numtagends = Util.countSub src tag_Close
    numtagbegins = sum (tags_All>~(Util.countSub src))



warnIfTagMismatches ctxmain filename (numtagbegins , numtagends) =
    if Util.startsWith filename Defaults.blokIndexPrefix || numtagbegins == numtagends
        then return () else
        let dropnum = (Util.startsWith filename maindirpath) |? (maindirpath~:length + 1) |! 0
            maindirpath = ctxmain~:Files.dirPath
        in putStrLn ("\t!!\tPotential syntax issue: "++
                     (show numtagends)++"x `|}` but "++(show numtagbegins)++"x `{*|`"++
                        "\n\t\tin your `"++(drop dropnum filename)++"`")



tag_Close = "|}"
tag_B = "{B|"
tag_C = "{C|"
tag_P = "{P|"
tag_T = "{T|"
tag_X = "{X|"
tags_All = [Tmpl.tag_B , Tmpl.tag_C , Tmpl.tag_P , Tmpl.tag_T , Tmpl.tag_X]
