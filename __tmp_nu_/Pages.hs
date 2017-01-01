{-# OPTIONS_GHC -Wall #-}
module Pages where

import qualified Bloks
import qualified Build
import qualified Defaults
import qualified Files
import qualified Proj
import qualified Tmpl
import Util ( (~:) , (>>~) , (>~) , (~.) )

import qualified System.FilePath
import qualified System.IO



processAll ctxmain ctxproj buildplan =
    let filenameexts = buildplan~:Build.outPages>~filenameext
        filenameext = Build.srcFile ~. Files.path ~. System.FilePath.takeExtension
        ctxtmpl = ctxproj~:Proj.setup~:Proj.tmpl
    in Tmpl.loadAll ctxmain ctxtmpl (ctxproj~:Proj.coreFiles) filenameexts >>= \tmplfinder
    -> let foreach buildtask =
            processPage ctxtmpl tmplfinder buildtask
    in buildplan~:Build.outPages>>~foreach



processPage ctxtmpl tmplfinder outjob =
    Files.writeTo dstfilepath (outjob~:Build.relPath) processcontent where
        dstfilepath = outjob~:Build.outPathBuild
        processcontent =
            System.IO.hFlush System.IO.stdout
            >> loadsrccontent >>= \pagesrc
            -> let
                blokname = outjob~:Build.blokName
                tmpl = tmplfinder$ System.FilePath.takeExtension dstfilepath
                outsrc = Tmpl.apply tmpl pagesrc
            in return (Tmpl.processSrcFully ctxtmpl blokname outsrc)
        loadsrccontent =
            let srcfilepath = outjob~:Build.srcFile~:Files.path
                blokindexname = Bloks.blokNameFromIndexPagePath srcfilepath
                blokindextmpl = tmplfinder Defaults.blokIndexPrefix
            in if null blokindexname then readFile srcfilepath
                else return (blokindextmpl~:Tmpl.srcFile~:Files.content)
