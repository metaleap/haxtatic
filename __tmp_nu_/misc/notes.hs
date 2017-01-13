--  reminder list comp:

let nums = [1,2,3,4,5]
in [ (-n) | n <- nums , let limit = 3 , n >= limit ]		==		[-3,-4,-5]





import qualified Data.Time.Calendar
main =
    go 1
    where
    go 0 =
        System.IO.hFlush System.IO.stdout
    go n =
        System.IO.hFlush System.IO.stdout
        >> Data.Time.Clock.getCurrentTime >>= \nowtime
        -> let r = toRational$ Data.Time.Clock.utctDayTime nowtime
        in System.IO.hFlush System.IO.stdout
        >> print ((Data.Time.Calendar.toModifiedJulianDay$ Data.Time.Clock.utctDay nowtime) :: Integer)
        >> print (Data.Ratio.numerator r)
        >> System.IO.hFlush System.IO.stdout
        >> print (Data.Ratio.denominator r)
        >> System.IO.hFlush System.IO.stdout
        >> putStrLn ("---------")
        >> System.IO.hFlush System.IO.stdout
        >> go (n - 1)






import qualified System.Random
import qualified Data.Map

fisherYatesStep :: System.Random.RandomGen g => (Data.Map.Map Int a, g) -> (Int, a) -> (Data.Map.Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((Data.Map.insert j x . Data.Map.insert i (m Data.Map.! j)) m, gen')
    where
    (j, gen') = System.Random.randomR (0, i) gen

fisherYates :: System.Random.RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l =
    toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
    where
    toElems (x, y) = (Data.Map.elems x, y)
    numerate = zip [1..]
    initial x _gen = (Data.Map.singleton 0 x, _gen)





main =
    let
        numf ln = foldr (*) 1 [1..ln]
        numr 0 = 1
        numr ln = ln * numr (ln - 1)

        go _ _ 0 v =
            print (v)
        go f x n r =
            (return (f x)) >>= go f (x-1) (n-1)
    in Data.Time.Clock.getCurrentTime
    >>= \starttime -> go numr 12345 123 0
    >> Data.Time.Clock.getCurrentTime
    >>= \midtime -> go numf 12345 123 0
    >> Data.Time.Clock.getCurrentTime
    >>= \donetime
    -> print (Util.duration starttime midtime)
    >> print (Util.duration midtime donetime)
