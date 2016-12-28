{-# OPTIONS_GHC -Wall #-}

module Files where

import qualified Util
import Util ( (~>) , (~.) )

import qualified Control.Monad
import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.FilePath
import System.FilePath ( (</>) )
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


filesInDir dir =
    System.Directory.doesDirectoryExist dir >>= \isdir
    -> if not isdir then return [] else
        System.Directory.listDirectory dir >>= \names
        -> Control.Monad.filterM isfile names where
            isfile = (dir</>) ~. System.Directory.doesFileExist >>= return


readOrCreate ctx relpath relpath2 defaultcontent =
    if null relpath then return NoFile else
    let filepath = System.FilePath.combine (ctx~>dirPath) relpath
        filepath2 = System.FilePath.combine (ctx~>dirPath) relpath2
    in System.Directory.doesFileExist filepath
    >>= \ isfile -> if isfile
        then
            System.Directory.getModificationTime filepath >>= \ modtime
            -> readFile filepath >>= \ filecontent
            -> return (File filepath filecontent modtime)
        else if Util.is relpath2
            then readOrCreate ctx relpath2 "" defaultcontent
            else
                System.Directory.createDirectoryIfMissing True (System.FilePath.takeDirectory filepath)
                >> writeTo filepath relpath defaultcontent
                >> return (File filepath defaultcontent (ctx~>nowTime))


rewrite file newmodtime newcontent =
    File {
        path = file~>path,
        content = newcontent, -- Util.fallback newcontent $content file,
        modTime = max newmodtime $modTime file
    }


writeTo filepath relpath filecontent =
    putStr ("   >> "++relpath++"  [ ")
    >> System.IO.hFlush System.IO.stdout
    >> writeFile filepath filecontent
    >> putStrLn "OK ]"
    >> System.IO.hFlush System.IO.stdout
