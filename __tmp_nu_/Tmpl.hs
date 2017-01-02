{-# OPTIONS_GHC -Wall #-}
module Tmpl where

import qualified Defaults
import qualified Files
import qualified Util
import Util ( (#) , (~|) , (~:) , (>>~) , (>~) , noNull )

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



_applychunkbegin = "{P|c"
_applychunkend = "ontent"++tag_Close
apply ctxtmpl pagesrc =
    let foreach ("","{P|c") = pagesrc
        foreach (why,"{P|c") = _applychunkbegin++why++_applychunkend
        foreach (str,"") = str
    in concat$ ctxtmpl~:chunks>~foreach



loadAll ctxmain ctxproc deffiles filenameexts =
    let fileexts = "":Defaults.blokIndexPrefix:exts where
        exts = Util.unique$ filenameexts ~| Util.noneOf ["",".html",".htm"]
        foreach "" = loadTmpl ctxproc "" $deffiles~:Defaults.htmlTemplateMain
        -- careful!! hackery! GHC doesn't do 'literal inlining', so ":B|" below must change whenever Defaults.blokIndexPrefix is
        foreach ":B|" = loadTmpl ctxproc Defaults.blokIndexPrefix $deffiles~:Defaults.htmlTemplateBlok
        foreach ext =
            let tmpath fn = "tmpl" </> (fn $".haxtmpl"++ext)
            in Files.readOrDefault False ctxmain (tmpath ((ctxmain~:Files.setupName)++))
                                    (tmpath Defaults.fileName_Pref) "" >>= loadTmpl ctxproc ext
    in fileexts>>~foreach >>= \templates
    -> let
        tmpldef = templates#0
        tmplfind "" = tmpldef
        tmplfind ".htm" = tmpldef
        tmplfind ".html" = tmpldef
        tmplfind ext =
            Data.Maybe.fromMaybe tmpldef $Data.List.find ((ext==).fileExt) templates
    in return tmplfind



loadTmpl ctxproc fileext tmpfile =
    let srcfile = Files.fullFrom tmpfile Util.dateTime0 srcpreprocessed
        srcpreprocessed = processSrcFully ctxproc "" (tmpfile~:Files.content)
        srcchunks = Util.splitUp [_applychunkbegin] _applychunkend srcpreprocessed
        tmpl = Template { fileExt = fileext, srcFile = srcfile,
                            chunks = if null srcchunks
                                then [("",_applychunkbegin)] else srcchunks }
    in return tmpl



processSrcFully ctxproc =
    Util.repeatedly.(processSrcJustOnce ctxproc)


processSrcJustOnce ctxproc bname src =
    let ptags = ctxproc~:processTags
    in concat$ (Util.splitUp ptags tag_Close src)>~foreach where
        foreach tag@(str , "{B|") =
            with tag ((ctxproc~:bTags) bname $str~:Util.trim)
        foreach tag@(str , "{C|") =
            with tag ((ctxproc~:cTags) $str~:Util.trim)
        foreach tag@(str , "{T|") =
            with tag ((ctxproc~:tTags) $str~:Util.trim)
        foreach tag@(str , "{X|") =
            with tag ((ctxproc~:xTags) $str~:Util.trim)
        foreach (str , "") =
            str
        foreach tag =
            with tag Nothing
        with (tagcontent,tagbegin) result =
            case result of
                Just output-> output
                Nothing-> tagbegin++tagcontent++tag_Close


tag_Close = "|}"
tag_B = "{B|"
tag_C = "{C|"
tag_P = "{P|"
tag_T = "{T|"
tag_X = "{X|"
tags_All = [Tmpl.tag_B , Tmpl.tag_C , Tmpl.tag_P , Tmpl.tag_T , Tmpl.tag_X]
