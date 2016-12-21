{-# OPTIONS_GHC -Wall #-}
module ProjDefaults where

import qualified Files

import qualified System.FilePath
import System.FilePath ( (</>) )



loadOrCreate dirpath projfilename custfilename = let
    projfilepath = dirpath </> (System.FilePath.takeFileName projfilename) -- turn a mistakenly supplied file-path back into just-name
    custfilepath = if null custfilename then "" else dirpath </> (System.FilePath.takeFileName custfilename)
    tmplfilepath = dirpath </> "default.haxtmpl.html"
    in
        putStrLn "1. Reading essential project files or (re)creating them..\n"
        >> Files.readOrCreate projfilepath "proj default content"
        >>= \projfilesrc ->
            Files.readOrCreate tmplfilepath "tmpl content"
            >>= \tmplfilesrc ->
                (if null custfilepath then (return "") else (Files.readOrCreate custfilepath ""))
                >>= \custfilesrc ->
                    return (Files.Defaults {
                            Files.project = Files.File projfilepath projfilesrc,
                            Files.projectOverwrites = Files.File custfilepath custfilesrc,
                            Files.htmlTemplate = Files.File tmplfilepath tmplfilesrc
                        })
