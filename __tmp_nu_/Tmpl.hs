{-# OPTIONS_GHC -Wall #-}
module Tmpl where

import qualified Defaults
import qualified Files
import qualified Util
import Util ( (#) , (~|) , (~:) , (>>~) , (>~) )

import qualified Data.List
import qualified Data.Maybe
import System.FilePath ( (</>) )



data Ctx
    = Processing {
        bTags :: String->String->String,
        tTags :: String->String
    }
    | Template {
        fileExt :: String,
        srcFile :: Files.File,
        chunks :: [(String , String)]
    }



_applychunkbegin = "{P{C"
_applychunkend = "ontent"++tag_Close
apply ctxtmpl pagesrc =
    let foreach ("","{P{C") = pagesrc
        foreach (wtf,"{P{C") = _applychunkbegin++wtf++_applychunkend
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
                            chunks = if null srcchunks then [("",_applychunkbegin)] else srcchunks }
    in return tmpl



processSrcFully ctxproc bname =
    Util.repeatedly (processSrcJustOnce ctxproc bname)


processSrcJustOnce ctxproc bname src =
    concat$ (Util.splitUp [tag_T,tag_B] tag_Close src)>~foreach where
        foreach (str , "{B{") =
            (ctxproc~:bTags) bname str
        foreach (str , "{T{") =
            (ctxproc~:tTags) str
        foreach (str , "") =
            str
        foreach (str , beg) =
            beg++str++tag_Close


tag_Close = "}}"
tag_B = "{B{"
tag_P = "{P{"
tag_T = "{T{"
tag_X = "{X{"
