{-# OPTIONS_GHC -Wall #-}

module Pages where

import qualified Bloks
import qualified Build
import qualified Defaults
import qualified Files
import qualified Proj
import qualified Util
import Util ( (~:) , (>>~) )

import qualified Control.Concurrent
import qualified System.FilePath
import System.FilePath ( (</>) )
import qualified System.IO



buildAll ctxproj buildplan =
	buildplan~:Build.outPages>>~foreach where
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
