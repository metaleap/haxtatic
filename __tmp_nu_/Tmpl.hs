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
        foreach (weird,"{P|:c") = _applychunkbegin++weird++_applychunkend -- the once-in-a-1000-years case .. someone went for {P|:cUriously persiStent:|} ?
        foreach (tmplsrc,_) = tmplsrc -- rest of template src as-is
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
                                then [(_applychunkmid,_applychunkbegin)] else srcchunks }
    in return tmpl



processSrcFully ctxproc =
    Util.repeatedly.(processSrcJustOnce ctxproc)


processSrcJustOnce ctxproc bname src =
    let ptags = ctxproc~:processTags
    in concat$ (Util.splitUp ptags tag_Close src)>~foreach where
        foreach tag@(_ , "{B|") =
            with tag ((ctxproc~:bTags) bname)
        foreach tag@(_ , "{C|") =
            with tag (ctxproc~:cTags)
        foreach tag@(_ , "{T|") =
            with tag (ctxproc~:tTags)
        foreach tag@(_ , "{X|") =
            with tag (ctxproc~:xTags)
        foreach (str , "") =
            str
        foreach tag =
            with tag (const Nothing)
        with (tagcontent,tagbegin) tagresolver =
            let noesc = not$ Util.startsWith tagcontent "``:"
                instr = if noesc then tagcontent else drop 3 tagcontent
                result = case (tagresolver instr) of
                            Just output-> output
                            Nothing-> tagbegin++tagcontent++tag_Close
            in if noesc then result
                else Util.crop 1 1 (show result)



tag_Close = "|}"
tag_B = "{B|"
tag_C = "{C|"
tag_P = "{P|"
tag_T = "{T|"
tag_X = "{X|"
tags_All = [Tmpl.tag_B , Tmpl.tag_C , Tmpl.tag_P , Tmpl.tag_T , Tmpl.tag_X]
