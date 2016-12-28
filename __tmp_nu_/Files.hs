{-# OPTIONS_GHC -Wall #-}

module Files where

import qualified Util
import Util ( (~>) )

import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.FilePath
import qualified System.IO



--  general project input file
data File = NoFile | File {
    path :: FilePath,
    content :: String,
    modTime :: Data.Time.Clock.UTCTime
} deriving (Eq)


data Ctx = Ctx {
    dirPath :: FilePath,
    nowTime :: Data.Time.Clock.UTCTime
}


readOrCreate ctx relpath defaultcontent =
    if null relpath then return NoFile else
    let filepath = System.FilePath.combine (ctx~>dirPath) relpath
    in System.Directory.doesFileExist filepath
    >>= \ isfile -> if isfile
        then
            System.Directory.getModificationTime filepath >>= \ modtime
            -> readFile filepath >>= \ filecontent
            -> return (File filepath filecontent modtime)
        else
            writeTo filepath relpath defaultcontent
            >> return (File filepath defaultcontent (ctx~>nowTime))


rewrite file newmodtime newcontent =
    File {
        path = file~>path,
        content = Util.fallback newcontent $content file,
        modTime = max newmodtime $modTime file
    }


writeTo filepath relpath filecontent =
    putStr ("   >> "++relpath++"  [ ")
    >> System.IO.hFlush System.IO.stdout
    >> writeFile filepath filecontent
    >> putStrLn "OK ]"
    >> System.IO.hFlush System.IO.stdout
