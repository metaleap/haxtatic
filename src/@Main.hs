module Main where

import Base
import qualified Lst

import qualified App
import qualified Build
import qualified Defaults
import qualified Files
import qualified Util

import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.Environment
import qualified System.IO
import qualified Text.Printf



main    :: IO ()
main
    = Data.Time.Clock.getCurrentTime >>= \starttime
    -> putStrLn "\n\n==== HAXTATIC ====\n"
    *> System.IO.hFlush System.IO.stdout
    *> System.Environment.getArgs >>= \cmdargs
    -> System.Directory.getCurrentDirectory >>= \curdir
    -> if null cmdargs
        then putStrLn "No project-directory path supplied.\n\
            \  For existing project: specify path to its current directory.\n\
            \  For a new project: specify path to its intended directory.\n    (I'll create it if missing and its parent isn't.)\n\n"
        else System.Directory.canonicalizePath (cmdargs@!0) >>= \dirpath
        --  ROLL UP SLEEVES
        -> let
            projfilename = (Defaults.fileName_Proj -|= cmdargs@?1)
            randseed' = (Util.dtInts starttime) ++ [curdir~>length , dirpath~>length , projfilename~>length]
            ctxmain = Files.AppContext {    Files.curDir = curdir,
                                            Files.dirPath = dirpath,
                                            Files.setupName = Defaults.setupName projfilename,
                                            Files.nowTime = starttime,
                                            Files.randSeed = randseed' >~ (+(randseed'@!1)) }
        --  GET TO WORK:
        in App.processAll ctxmain projfilename (drop 2 cmdargs)
        --  REMINISCE:

        >>= \(buildplan , warnpages , hintpages , numoutfiles , numxmls , timeinitdone , timecopydone , timeprocdone , timexmldone)
        -> Data.Time.Clock.getCurrentTime >>= \endtime
        -> let
            timetaken = Util.duration starttime endtime
            showtime = showtime' 3
            showtime' numdig difftime =
                let overaminute = difftime > 60
                in (overaminute |? (difftime / 60) |! difftime)
                    ~> show ~> ( Util.cropOn1st '.' numdig ['0'] ((++(overaminute |? "m" |! "s")) . (Lst.trimEndEq ['.'])) )
            showavg 0 _ _ = ""
            showavg l t1 t2 = Text.Printf.printf " (%ux ~%s)" l (showtime' 4 ((Util.duration t1 t2) / (fromIntegral l)))
        in Text.Printf.printf "\n\nWrote %u files in %s:" numoutfiles (showtime timetaken)
        *> Text.Printf.printf "\n\t%s pre-templating & planning"
                                (showtime$ Util.duration starttime timeinitdone)
        *> Text.Printf.printf "\n\t%s page templating & generation%s"
                                (showtime$ Util.duration timecopydone timeprocdone)
                                (showavg (buildplan-:Build.outPages~>length) timecopydone timeprocdone)
        *> Text.Printf.printf "\n\t%s XML file generation%s"
                                (showtime$ Util.duration timeprocdone timexmldone)
                                (showavg numxmls timeprocdone timexmldone)
        *> Text.Printf.printf "\n\t%s misc. & file-copying%s"
                                (showtime$ ((Util.duration timeinitdone timecopydone) + (Util.duration timexmldone endtime)))
                                (showavg (buildplan-:Build.outStatics~>length) timeprocdone endtime)
        *> if has warnpages
            then putStrLn ("\n\n" ++ (Lst.joined "\n\t!>\t" ("Apparent {!| ERROR MESSAGES |!} were rendered into:":warnpages)) ++ "\n\n")
            else if has hintpages
            then putStrLn ("\n\n" ++ (Lst.joined "\n\t?>\t" ("DON'T MISS the (potentially critical) notices above for:":hintpages)) ++ "\n\n")
            else putStrLn ("\n\n==== Bye now! ====\n\n")
        *> System.IO.hFlush System.IO.stdout
