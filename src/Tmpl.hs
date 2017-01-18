{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Tmpl where

import Base
import qualified Defaults
import qualified Files
import qualified Util

import qualified Data.List
import qualified Data.Time.Clock
import System.FilePath ( (</>) )
import qualified Text.Printf



data CtxProc
    = ProcessingContext {
        bTagHandler :: String -> String -> Maybe String,
        cTagHandler :: String -> Maybe String,
        tTagHandler :: String -> Maybe String,
        xTagHandler :: Maybe CtxPage -> String -> Maybe String,
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
        pTagHandler :: String -> Maybe String,
        pVars :: Util.StringPairs,
        pDate :: Data.Time.Clock.UTCTime,
        htmlInners :: String->[String],
        htmlInner1st :: String->String->String,
        tmpl :: CtxTmpl,
        cachedRenderSansTmpl :: String,
        lookupCachedPageRender :: FilePath->Maybe CtxPage,
        allPagesFiles :: [(FilePath , Files.File)],
        randSeed :: [Int]
    }



_applychunkbegin = tag_P
_applychunkmid = ":content:"
_applychunkend = tag_Close
apply ctxtmpl ctxpage pagesrc =
    -- concatMap foreach (ctxtmpl-:chunks)
    concat$ ctxtmpl-:chunks >~ foreach
    where
    foreach (":content:" , "{P|") =
        pagesrc
    foreach other@(_ , "{P|") =
        processTag ptaghandler other
    foreach (tmplsrc , _) =
        tmplsrc
    ptaghandler = ctxpage-:pTagHandler



fixParseStr fulltextfieldname pstr =
    if i < 0 then pstr else
    (take i pstr) ++ fulltextfieldname ++ "=" ++ (pstr ~> ((drop$ i+l) ~. Util.trim ~. show))
        where i = Util.indexOfSub pstr (fulltextfieldname++"=>") ; l = 2 + fulltextfieldname~>length



loadAll ctxmain ctxproc deffiles filenameexts htmlequivexts =
    let foreach fileext
            | null fileext
            = loadTmpl ctxmain ctxproc "" $deffiles-:Defaults.htmlTemplateMain
            | otherwise
            = let tmplpath name = "tmpl" </> (name $".haxtmpl"++fileext)
                in Files.readOrDefault False ctxmain (tmplpath ((ctxmain-:Files.setupName)++))
                    --  fallback path tmpl/default.haxtmpl.<fileext>
                    (tmplpath Defaults.fileName_Pref)
                        --  fallback template content: `{P|:content:|}`
                        (_applychunkbegin++_applychunkmid++_applychunkend)
                    >>= loadTmpl ctxmain ctxproc fileext
        fileexts = "":otherexts where
            otherexts = (Util.unique filenameexts) ~| Util.noneOf htmlequivexts
    in fileexts>>~foreach
    >>= \loadedtemplates
    -> let
        tmpldef = loadedtemplates@!0
        tmplfind "" = tmpldef
        tmplfind ".html" = tmpldef
        tmplfind ".htm" = tmpldef
        tmplfind ext =
            elem ext htmlequivexts |? tmpldef |!
                tmpldef -|= (Data.List.find ((ext==).fileExt) loadedtemplates)
    in return tmplfind



loadTmpl ctxmain ctxproc fileext tmpfile =
    warnIfTagMismatches ctxmain (srcfile-:Files.path) (tagMismatches rawsrc)
    >> return TemplateContext {
                fileExt = fileext, srcFile = srcfile, chunks = srcchunks
            }
    where
    srcfile = tmpfile { Files.content = srcpreprocessed }
    srcchunks = Util.splitUp Util.trim [_applychunkbegin] _applychunkend srcpreprocessed
    srcpreprocessed = processSrcFully ctxproc Nothing rawsrc
    rawsrc = (tmpfile-:Files.content)



processSrcFully ctxproc ctxpage =
    Util.repeatedly process
    where
    preserveunprocessedtag = const Nothing
    splitup = Util.splitUp Util.trim (ctxproc-:processTags) tag_Close
    _c = ctxproc-:cTagHandler ; _t = ctxproc-:tTagHandler
    _b = (ctxproc-:bTagHandler) (ctxpage-:(blokName =|- ""))
    _p = ctxpage-:(pTagHandler =|- preserveunprocessedtag)
    _x = (ctxproc-:xTagHandler) ctxpage
    process src =
        -- concatMap foreach (splitup src)
        concat$ (splitup src) >~foreach
        where
        foreach (srccontent , "") =
            srccontent
        foreach (tagcontent , tagbegin) =
            processTag taghandler (tagcontent , tagbegin)
            where
            taghandler
                |(tagbegin==tag_T)= _t
                |(tagbegin==tag_X)= _x
                |(tagbegin==tag_B)= _b
                |(tagbegin==tag_P)= _p
                |(tagbegin==tag_C)= _c
                |(otherwise)= preserveunprocessedtag



processTag taghandler (tagcontent , tagbegin) =
    noesc |? result |! (show result) ~> (Util.crop 1 1)
    where
    noesc = not$ Util.startsWith tagcontent "``:"
    taginner = noesc |? tagcontent |! drop 3 tagcontent
    result = case (taghandler taginner) of
        Just output-> output
        Nothing-> tagbegin++tagcontent++tag_Close


processXtagDelayed taghandler tagcontent =
    processTag taghandler (tagcontent , tag_X)



tagMismatches src =
    Util.countSubVsSubs src (tag_Close , tags_All)

warnIfTagMismatches ctxmain filename (numtagends , numtagbegins) =
    if Util.startsWith filename Defaults.blokIndexPrefix || numtagbegins == numtagends
        then return ()
        else
        let maindirpath = ctxmain-:Files.dirPath
            trim = (Util.startsWith filename maindirpath) |? (drop$ maindirpath~>length + 1) |! id
        in Text.Printf.printf "...\t<~\tPotential syntax issue: %ux `|}` but %ux `{*|`\n\t\t\tin `%s`\n"
                                (numtagends::Int) (numtagbegins::Int) (trim filename)



tag_Close = "|}"
tag_B = "{B|"
tag_C = "{C|"
tag_P = "{P|"
tag_T = "{T|"
tag_X = "{X|"
tags_All = [Tmpl.tag_B , Tmpl.tag_C , Tmpl.tag_P , Tmpl.tag_T , Tmpl.tag_X]