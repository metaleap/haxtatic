{-# OPTIONS_GHC -Wall #-}
module Tmpl where

import Base
import qualified Defaults
import qualified Files
import qualified Util

import qualified Data.List
import System.FilePath ( (</>) )



data CtxProc
    = ProcessingContext {
        bTags :: String->String->Maybe String,
        cTags :: String->Maybe String,
        tTags :: String->Maybe String,
        xTags :: Maybe CtxPage->String->Maybe String,
        processTags :: [String]
    }

data CtxTmpl
    = TemplateContext {
        fileExt :: String,
        srcFile :: Files.File,
        chunks :: [(String , String)]
    }



data CtxPage
    = PageContext {
        blokName :: String,
        pTags :: String->Maybe String,
        pVars :: Util.StringPairs,
        tmpl :: CtxTmpl
    }



_applychunkbegin = "{P|"
_applychunkmid = ":content:"
_applychunkend = tag_Close
apply ctxtmpl ctxpage pagesrc =
    concat$ ctxtmpl.:chunks >~ foreach
    where
    foreach (":content:" , "{P|") =
        pagesrc
    foreach other@(_ , "{P|") =
        processTag ptags other
    foreach (tmplsrc , _) =
        tmplsrc
    ptags = ctxpage.:pTags



loadAll ctxmain ctxproc deffiles filenameexts htmlequivexts =
    let foreach fileext
            | null fileext
            = loadTmpl ctxmain ctxproc "" $deffiles.:Defaults.htmlTemplateMain
            | fileext==Defaults.blokIndexPrefix
            =  loadTmpl ctxmain ctxproc Defaults.blokIndexPrefix $deffiles.:Defaults.htmlTemplateBlok
            | otherwise
            = let tmplpath name = "tmpl" </> (name $".haxtmpl"++fileext)
                in Files.readOrDefault False ctxmain (tmplpath ((ctxmain.:Files.setupName)++))
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
                tmpldef -|= (Data.List.find ((ext==).fileExt) loadedtemplates)
    in return tmplfind



loadTmpl ctxmain ctxproc fileext tmpfile =
    warnIfTagMismatches ctxmain (srcfile.:Files.path) (tagMismatches rawsrc)
    >> return TemplateContext {
                fileExt = fileext, srcFile = srcfile, chunks = srcchunks
            }
    where
    srcfile = Files.fullFrom tmpfile Util.dateTime0 srcpreprocessed
    srcchunks = Util.splitUp [_applychunkbegin] _applychunkend srcpreprocessed
    srcpreprocessed = processSrcFully ctxproc Nothing rawsrc
    rawsrc = (tmpfile.:Files.content)



processSrcFully ctxproc ctxpage =
    Util.repeatedly process
    where
    processtags = case ctxpage of
                    Nothing -> (ctxproc.:processTags) ~|(/=tag_P)
                    Just _ -> ctxproc.:processTags
    process src =
        concat$ (Util.splitUp processtags tag_Close src)>~foreach
        where
        foreach (srccontent , "") =
            srccontent
        foreach (tagcontent , tagbegin) =
            processTag tagresolver (tagcontent , tagbegin)
            where
            tagresolver
                |(tagbegin==tag_B)= (ctxproc.:bTags) blokname
                |(tagbegin==tag_C)= ctxproc.:cTags
                |(tagbegin==tag_T)= ctxproc.:tTags
                |(tagbegin==tag_X)= (ctxproc.:xTags) ctxpage
                |(tagbegin==tag_P)= ctxpage.:(pTags =|- preserveunprocessedtag)
                |(otherwise)= preserveunprocessedtag
            blokname = ctxpage.:(blokName =|- "")
            preserveunprocessedtag = const Nothing



processTag tagresolver (tagcontent , tagbegin) =
    noesc |? result |! (show result) ~> (Util.crop 1 1)
    where
    noesc = not$ Util.startsWith tagcontent "``:"
    taginner = noesc |? tagcontent |! drop 3 tagcontent
    result = case (tagresolver taginner) of
        Just output-> output
        Nothing-> tagbegin++tagcontent++tag_Close



tagMismatches src =
    Util.countSubVsSubs src (tag_Close , tags_All)


warnIfTagMismatches ctxmain filename (numtagends , numtagbegins) =
    if Util.startsWith filename Defaults.blokIndexPrefix || numtagbegins == numtagends
        then return () else
        let drops = (Util.startsWith filename maindirpath) |? (maindirpath~>length + 1) |! 0
            maindirpath = ctxmain.:Files.dirPath
        in putStrLn ("\t!?\tPotential syntax issue: "++
                     (show numtagends)++"x `|}` but "++(show numtagbegins)++"x `{*|`"++
                        "\n\t\tin your `"++(drop drops filename)++"`")



tag_Close = "|}"
tag_B = "{B|"
tag_C = "{C|"
tag_P = "{P|"
tag_T = "{T|"
tag_X = "{X|"
tags_All = [Tmpl.tag_B , Tmpl.tag_C , Tmpl.tag_P , Tmpl.tag_T , Tmpl.tag_X]
