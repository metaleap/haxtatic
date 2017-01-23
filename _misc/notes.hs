https://channel9.msdn.com/tags/monad
https://ocharles.org.uk/blog/posts/2014-12-08-type-operators.html


Typed REST
http://bobkonf.de/2016/loeh-servant.html

Lenses, Folds, and Traversals
https://www.youtube.com/watch?v=cefnmjtAolY&feature=youtu.be&hd=1
https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation

Monad transformer stacks / MTL
https://www.youtube.com/watch?v=GZPup5Iuaqw&feature=youtu.be
https://www.youtube.com/watch?v=pzouxmWiemg

STG
https://www.youtube.com/watch?v=-MFk7PIKYsg&t=1s

https://github.com/ChrisPenner/Advent-Of-Code-Polyglot/tree/master/2016

https://hackhands.com/lazy-evaluation-works-haskell/
https://bartoszmilewski.com/2011/03/14/monads-for-the-curious-programmer-part-2/
https://channel9.msdn.com/Blogs/Charles/Brian-Beckman-Monads-Monoids-and-Mort

frp-cuboid
https://www.youtube.com/watch?v=-IpE0CyHK7Q&feature=youtu.be

data modeling
https://www.youtube.com/watch?v=p-NBJm0kIYU

https://www.reddit.com/r/haskell/comments/5ksagy/what_was_the_topmost_thing_you_learned_about/



http://www.flexboxpatterns.com/



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
