{-# OPTIONS_GHC -Wall #-}
module Pages where

import qualified Bloks
import qualified Build
import qualified Defaults
import qualified Files
import qualified Proj
import qualified Util
import Util ( noNull , (~|) , (~:) , (>>~) , (>~) , (~.) , (#) )

import qualified Control.Concurrent
import qualified System.Directory
import qualified System.FilePath
import System.FilePath ( (</>) )
import qualified System.IO



data Template = NoTmpl | Tmpl {
    srcFile :: Files.File
}



buildAll ctxmain ctxproj buildplan =
    loadTemplates ctxmain ctxproj buildplan >>= \templates
    -> print (templates~:length)
    >> buildplan~:Build.outPages>>~foreach where
        foreach buildtask =
            buildpage buildtask
        buildpage = buildPage ctxproj



buildPage ctxproj outjob =
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
    let
    fileexts = Util.uniques$ buildplan~:Build.outPages>~fileext
    fileext = Build.srcFile ~. Files.path ~. System.FilePath.takeExtension
    foreach ".html" = foreach ""
    foreach ".htm" = foreach ""
    foreach "" = return Tmpl { srcFile = ctxproj~:Proj.coreFiles~:Defaults.htmlTemplateMain }
    foreach ext =
        let tp fn = "tmpl" </> (fn $".haxtmpl"++ext)
        in Files.readOrDefault False ctxmain (tp (ctxproj~:Proj.setupName++))
            (tp Defaults.fileName_Pref) "" >>= loadTemplate
    in fileexts>>~foreach



loadTemplate tmplfile =
    return Tmpl { srcFile = tmplfile }
