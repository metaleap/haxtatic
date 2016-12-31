{-# OPTIONS_GHC -Wall #-}

module Pages where

import qualified Files
import qualified Proj
import qualified Util
import Util ( (~:) , (>~) , (~|) , (#) )

import qualified Data.Maybe
import qualified System.FilePath
import System.FilePath ( (</>) )



customContentDateFromFileName projcfg (filepath , srcfile) =
    let
        (filedir , filename) = System.FilePath.splitFileName filepath
        (datepart , fnrest) = System.FilePath.splitExtensions filename
        datemaybe = Proj.dtStr2Utc projcfg "hx_pagedate" datepart
    in case datemaybe of
        Just customdate->
            ( (Files.saneDirPath filedir) </> (Util.trimStart' ['.'] fnrest) , customdate )
        Nothing->
            ( filepath , srcfile~:Files.modTime )
