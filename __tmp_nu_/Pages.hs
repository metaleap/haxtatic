{-# OPTIONS_GHC -Wall #-}
module Pages where

import qualified Bloks
import qualified Build
import qualified Defaults
import qualified Files
import qualified Proj
import qualified Util
import Util ( noNull , (~|) , (~:) , (>>~) , (>>|) , (>~) , (~.) , (#) )

import qualified Control.Concurrent
import Control.Monad
import qualified Data.List
import qualified Data.Maybe
import qualified System.Directory
import qualified System.FilePath
import System.FilePath ( (</>) )
import qualified System.IO



data Template = Tmpl {
    fileExt :: String,
    srcFile :: Files.File
}



buildAll ctxmain ctxproj buildplan =
    loadTemplates ctxmain ctxproj buildplan >>= \templates
    -> let
        tmpldef = templates#0 ; tmplfind "" = tmpldef
        tmplfind ".htm" = tmpldef ; tmplfind ".html" = tmpldef
        tmplfind ext =
            Data.Maybe.fromMaybe tmpldef $Data.List.find ((ext==).fileExt) templates
        foreach buildtask =
            buildPage ctxproj tmplfind buildtask
    in print (templates~:length)
    >> buildplan~:Build.outPages>>~foreach



buildPage ctxproj tmplfinder outjob =
    Files.writeTo dstfilepath (outjob~:Build.relPath) loadcontent where
        dstfilepath = outjob~:Build.outPathBuild
        srcfilepath = outjob~:Build.srcFile~:Files.path
        blokindexname = Bloks.blokNameFromIndexPagePath srcfilepath
        loadcontent =
            System.IO.hFlush System.IO.stdout
            >> Control.Concurrent.threadDelay 234567
            >> System.IO.hFlush System.IO.stdout

            >> if null blokindexname
                then readFile srcfilepath
                else return blokindexname



loadTemplates ctxmain ctxproj buildplan =
    let foreach "" = loadTmpl ctxproj "" $ctxproj~:Proj.coreFiles~:Defaults.htmlTemplateMain
        foreach ":blok" = loadTmpl ctxproj ":blok" $ctxproj~:Proj.coreFiles~:Defaults.htmlTemplateBlok
        foreach ext =
            let tmpath fn = "tmpl" </> (fn $".haxtmpl"++ext)
            in Files.readOrDefault False ctxmain (tmpath (ctxproj~:Proj.setupName++))
                                    (tmpath Defaults.fileName_Pref) "" >>= loadTmpl ctxproj ext
        fileext = Build.srcFile ~. Files.path ~. System.FilePath.takeExtension
        fileexts = Util.uniques$ buildplan~:Build.outPages>~fileext ~|Util.noneOf ["",".html",".htm"]
    in ("":":blok":fileexts)>>~foreach



loadTmpl ctxproj fileext tmplfile =
    let
        srcpreprocessed = Proj.processSrcFully (ctxproj~:Proj.setup) (tmplfile~:Files.content)
        srcfile = Files.fullFrom tmplfile Util.dateTime0 srcpreprocessed
    in
        putStrLn ("\n\n\n\t\t"++fileext++"\t\t"++(srcfile~:Files.path))
        >> print (srcpreprocessed)
        >> return Tmpl { fileExt = fileext, srcFile = srcfile }
