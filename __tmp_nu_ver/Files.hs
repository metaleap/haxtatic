{-# OPTIONS_GHC -Wall #-}
module Files where

import qualified System.Directory
import qualified System.IO


--  general project input file
data File = File {
    path :: FilePath,
    raw :: String
} deriving (Show)


data Defaults = Defaults {
    project :: File,
    projectOverwrites :: File,
    htmlTemplate :: File
} deriving (Show)




readOrCreate filepath defaultcontent =
    System.Directory.doesFileExist filepath
    >>= \exists -> if exists then readFile filepath else
        writeTo filepath defaultcontent >> return defaultcontent


writeTo filepath content =
    putStr ("\t>> "++filepath++"  [ ")
    >> System.IO.hFlush System.IO.stdout
    >> writeFile filepath content
    >> putStrLn "OK ]"
