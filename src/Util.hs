{-# OPTIONS_GHC -Wmissing-signatures #-}
module Util where

import Base
import qualified Lst
import qualified Str

import qualified Data.Char
import qualified Data.List
import qualified Data.Ratio
import qualified Data.Set
import qualified Data.Time.Calendar
import qualified Data.Time.Clock



_intmin :: Int
_intmin = minBound::Int

dateTime0   :: Data.Time.Clock.UTCTime
dateTime0   = Data.Time.Clock.UTCTime {
                    Data.Time.Clock.utctDay = Data.Time.Calendar.ModifiedJulianDay {
                                                Data.Time.Calendar.toModifiedJulianDay = 0 },
                    Data.Time.Clock.utctDayTime = 0
                }



dtInts  ::  Data.Time.Clock.UTCTime
        ->  [Int]
dtInts  utctime
    = [ modifiedjulianday , daytimenumer , daytimedenom ]
    where
    modifiedjulianday = fromInteger$ Data.Time.Calendar.toModifiedJulianDay
                                            (Data.Time.Clock.utctDay utctime)
    daytimenumer = fromInteger$ Data.Ratio.numerator daytime
    daytimedenom = fromInteger$ Data.Ratio.denominator daytime
    daytime = toRational (Data.Time.Clock.utctDayTime utctime)




onBoth  ::  (a->b)  ->  (a,a)
        ->  (b,b)
onBoth  = duo . dupl


onFsts  ::  (i1->i2->o)  ->  (i1,_1)  ->  (i2,_2)
        ->  o
onFsts  fn (fst1 , _) (fst2 , _)
    = fn fst1 fst2


onSnds  ::  (i1->i2->o)  ->  (_1,i1)  ->  (_2,i2)
        ->  o
onSnds  fn (_ , snd1) (_ , snd2)
    = fn snd1 snd2




butNot  ::  (Eq e)
        =>  e  ->  e  ->  e
        ->  e
butNot  unwanted fallback value
    = if value==unwanted then fallback else value


ifIs    ::  [a]  ->  ([a]->[b])
        ->  [b]
ifIs    [] _
    = []
ifIs    val func
    = func val


eitherOne   ::  (Eq a)
            =>  a  ->  a  ->  a
            ->  a
eitherOne   value yay nay
    = if value==yay then yay else nay



via ::  (Applicative a)
    =>  a i  ->  o
    ->  a o
--  use it to `>>=` a value over to fn but then
--  discard its result and return said value instead
via fn
    = ((*>)fn).pure



clamp   ::  (Int,Int)  ->  Int
        ->  Int
clamp   (minval , maxval)
    = (max minval) . (min maxval)


duration    ::  Data.Time.Clock.UTCTime  ->  Data.Time.Clock.UTCTime
            ->  Data.Time.Clock.NominalDiffTime
duration    = flip Data.Time.Clock.diffUTCTime


cropOn1st   ::  Char  ->  Int  ->  [Char]  ->  (String->String)  ->  String
            ->  String
cropOn1st   delim cropafter trimitemsafterdelim oncrop list
    = let i = indexOf delim list
    in if i < 0 then list else
        (oncrop . (Lst.trimEndEq trimitemsafterdelim) . (take (i+cropafter))) list


countSub    ::  String  ->  String
            ->  Int
countSub    _ []
    = 0
countSub    [] _
    = 0
countSub    list sub
    = foldr each 0 (Data.List.tails list) where
        each listtail counter =
            if Lst.isPrefixOf sub listtail then counter + 1 else counter


countAnySubs    ::  String  ->  [String]
                ->  Int
--  equivalent to, but faster than:
--      sum (map (countSub list) subs)
countAnySubs    list subs
    = foldr each 0 (Data.List.tails list) where
        each listtail counter =
            if any isprefix subs then counter + 1 else counter where
            isprefix sub = Lst.isPrefixOf sub listtail


countSubVsSubs  ::  String  ->  (String,[String])
                ->  (Int,Int)
--  equivalent to, but faster than:
--      (countSub list sub, countAnySubs list subs)
countSubVsSubs  list (sub,subs)
    = foldr each (0,0) (Data.List.tails list) where
        each listtail counter =
            let isprefix sublist = Lst.isPrefixOf sublist listtail
                isp1 = isprefix sub
                isp2 = any isprefix subs
            in if isp1 || isp2
                then let (c1,c2) = counter in
                        (if isp1 then c1 + 1 else c1,
                        if isp2 then c2 + 1 else c2)
                else counter


fstBegins   ::  (Eq a)
            =>  a  ->  ([a],b)
            ->  Bool
fstBegins   item ((this:_),_)   = item==this
fstBegins   _ _                 = False


lookup  ::  (Eq a)
        =>  a  ->  b  ->  [(a,b)]
        ->  b
lookup  key defval
    = (defval -|=) . (Lst.lookup key)


subAt   ::  Int  ->  Int  ->  [a]
        ->  [a]
subAt   start len
    = (take len) . (drop start)


substitute  ::  (Eq a)
            =>  a  ->  a  ->  [a]
            ->  [a]
substitute  old new
    | old==new  = id
    | otherwise = fmap$ \ item -> if item==old then new else item


substitute' ::  (Eq a)
            =>  [a] -> a -> [a]
            -> [a]
substitute' olds new
    = fmap$ \ item -> elem item olds |? new |! item



noNils  ::  [[a]]
        ->  [[a]]
noNils  list
    = [item | item <- list, has item]


forNoNilsEach   ::  ([a]->b)  ->  [[a]]
                ->  [b]
forNoNilsEach   _ []
    = []
forNoNilsEach   func ([]:more)
    = forNoNilsEach func more
forNoNilsEach   func (item:more)
    = (func item):(forNoNilsEach func more)


noNilFsts   ::  [([a] , b)]
            ->  [([a] , b)]
noNilFsts   []
    = []
noNilFsts   (([],_):more)
    = noNilFsts more
noNilFsts   (item:more)
    = item:(noNilFsts more)


keepNoNilFsts   ::  (a->([b] , c))  ->  [a]
                ->  [([b] , c)]
keepNoNilFsts   _ []
    = []
    -- keepNoNilFsts func (([],_):more) = keepNoNilFsts func more
keepNoNilFsts   func (item:more)
    | has f     = (f,s) : keepNoNilFsts func more
    | otherwise = keepNoNilFsts func more
    where (f,s) = func item



sortDesc    :: (Ord a)
            =>  [a]
            ->  [a]
sortDesc    = Data.List.sortBy (flip compare)


uniqueO ::  (Eq a)
        =>  [a]
        ->  [a]
uniqueO = Data.List.nub


uniqueU ::  (Ord a)
        =>  [a]
        ->  [a]
--  http://stackoverflow.com/a/16109302
uniqueU = (map (@!0)) . Data.List.group . Data.List.sort


unique' ::  (Ord a)
        =>  [a]
        ->  [a]
--  http://stackoverflow.com/a/16111081
unique'
    = halp Data.Set.empty
    where
    halp _ [] = []
    halp set (this:rest) =
        if Data.Set.member this set
            then halp set rest
            else this : (halp (Data.Set.insert this set) rest)


unique  ::  (Ord a)
        =>  [a]
        ->  [a]
--  https://github.com/quchen/articles/blob/master/fbut.md#nub
unique  list
    = foldr go (const []) list Data.Set.empty where
        go x xs cache
            | Data.Set.member x cache   = xs cache
            | otherwise                 = x : xs (Data.Set.insert x cache)


uniqueBy::  (a->a->Bool)  ->  [a]
            ->  [a]
uniqueBy    = Data.List.nubBy


uniqueFst   ::  (Eq e)
            =>  [(e , a)]
            ->  [(e , a)]
uniqueFst   = uniqueBy (onFsts (==))


uniqueSnd   :: (Eq e)
            =>  [(a , e)]
            ->  [(a , e)]
uniqueSnd   = uniqueBy (onSnds (==))



mergeDuplFsts   ::  (String->String->Bool)  ->  ([String]->String)  ->  Str.Pairs
                ->  Str.Pairs
mergeDuplFsts   isduplicate mergevalues
    = mergeduplfsts where
        mergeduplfsts [] = []
        mergeduplfsts ((dispair@(disname,disval)):morepairs) =
            let (duplpairs , otherpairs) = morepairs ~> (Data.List.partition $(isduplicate disname).fst)
            in if null duplpairs
                then dispair : (mergeduplfsts morepairs)
                else (disname , mergevalues$ disval:(duplpairs>~snd)) : (mergeduplfsts otherpairs)




indexOf ::  (Eq a)
        =>  a  ->  [a]
        ->  Int
indexOf _ []
    = _intmin
indexOf item (this:rest)
    = if this==item then 0 else
        (1 + (indexOf item rest))



_indexof_droptil :: (Eq a)=>  a  ->  Int  ->  [a]  ->  (Int , [a])
_indexof_droptil delim = _indexof_droptil' (delim==)

_indexof_droptil' ::  (a->Bool)  ->  Int  ->  [a]  ->  (Int , [a])
_indexof_droptil' _ _ [] =
    (_intmin , [])
_indexof_droptil' check counter list@(this:rest) =
    if check this then (counter , list)
        else _indexof_droptil' check (counter + 1) rest


indexOf1st  ::  (Eq a)
            =>  [a]  ->  [a]
            ->  Int
indexOf1st  items list
    = i where
        (i , _) = _indexof_droptil' (`elem` items) 0 list

indexOfSub  ::  String  ->  String
            ->  Int
indexOfSub  [] _
    = _intmin
indexOfSub  _ []
    = _intmin
indexOfSub  haystack needle
    = let (startindex , _haystack) = _indexof_droptil (needle@!0) 0 haystack
    in if startindex<0 then startindex else
        startindex + indexofsub _haystack needle
        where
        indexofsub [] _ =
            _intmin
        indexofsub list@(_:rest) sub =
            if Lst.isPrefixOf sub list then 0
                else (1 + indexofsub rest sub)
--  indexOfSub list@(_:rest) sub =
--      if Lst.isPrefixOf sub list then 0
--          else (1 + indexOfSub rest sub)


indexOfSubs1st  ::  [String]  ->  String
                ->  (Int , String)
indexOfSubs1st  [] _
    = (_intmin , [] )
indexOfSubs1st  _ []
    = (_intmin , [] )
indexOfSubs1st  subs str
    = if startindex<0 || null iidxs || index<0
        then ( _intmin , [] )
        else ( index+startindex , subs@!i )
    where
    (startindex , _haystack) = _indexof_droptil' (`elem` startchars) 0 str
    startchars = unique$ subs>~(@!0)
    iidxs = isubs >~ (>~ (indexOfSub _haystack)) ~|(>=0).snd
    (i,index) = Data.List.minimumBy (onSnds compare) iidxs
    isubs = Lst.indexed subs



lastIndexOfSub  ::  String  ->  String
                ->  Int
lastIndexOfSub  revstr revsub
    | idx < 0   = idx
    | otherwise = revstr~>length - revsub~>length - idx
    where idx = indexOfSub revstr revsub



replaceSub  ::  (String , String)  ->  String
            ->  String
replaceSub  ([] , _)
    = id
replaceSub  (old , new)
    = replsub where
        replsub [] = []
        replsub list@(this : more)
            | (isprefix list) = withnew list
            | (otherwise) = this : replsub more
        withnew = ((new ++) . replsub . dropold)
        dropold = drop $old~>length
        isprefix = Lst.isPrefixOf old


replaceSub' ::  (String , String)  ->  String
            ->  String
replaceSub' ([] , _)
    = id
replaceSub' (old , new)
    = replsub where
        replsub [] = []
        replsub str =
            if str==old then new
            else replrec (indexOfSub str old , old , oldlen) str
        replrec = _replcore replsub (const new)
        oldlen = old~>length


replaceSubsFew  ::  Str.Pairs  ->  String
                ->  String
replaceSubsFew  []
    = id
replaceSubsFew  [replpair]
    = replaceSub replpair
replaceSubsFew  replpairs
    = replall where
        replall [] = []
        replall str =
            replrec (idx , old , old~>length) str
            where
            (idx,old) = indexOfSubs1st olds str
        (olds,_) = unzip replpairs
        replrec = _replcore replall (tonew replpairs)
        tonew ((replold , replnew):replmore) curold =
            if curold==replold then replnew else tonew replmore curold
        tonew _ _ =     -- impossible, or a freshly introduced bug: `curold` did
            undefined   -- not exist as a `replold` (ie. fst) in `replpairs`?!


replaceSubsMany ::  Str.Pairs  ->  String
                ->  String
replaceSubsMany [] list
    = list
replaceSubsMany [replpair] list
    = replaceSub replpair list
replaceSubsMany replpairs list
    = Data.List.foldl' replsub list replpairs where
        replsub = flip replaceSub


replaceSubsBy   ::  [String]  ->  (String->String)  ->  String
                ->  String
replaceSubsBy   [] _
    = id
replaceSubsBy   olds tonew
    = replby where
        replby [] = []
        replby str =
            let (idx , old) = indexOfSubs1st olds str
            in replrec (idx , old , old~>length) str
        replrec = _replcore replby tonew


replaceWith ::  (Char , String)  ->  String
            ->  String
replaceWith repl@(old , new) list
    = let (idx , rest) = _indexof_droptil old 0 list
    in if idx < 0 then list else
        (take idx list) ++ new ++ (replaceWith repl (drop 1 rest))


_replcore   ::  (String->String)  ->  (String->String)  ->  (Int , String , Int)  ->  String
            ->  String
_replcore   recurse tonew (idx , old , oldlen) str
    = if idx<0 then str else
        let pre = take idx str
            rest = drop (idx + oldlen) str
        in pre ++ tonew old ++ recurse rest



splitOn1st  ::  (Eq a)
            =>  a  ->  [a]
            ->  ([a] , [a])
splitOn1st delim
    = splitOn1st' (delim==)

splitOn1st' ::  (a->Bool)  ->  [a]
            ->  ([a] , [a])
splitOn1st' check list
    | i < 0     = (list , [])
    | otherwise = (take i list , drop 1 rest)
    where (i , rest) = _indexof_droptil' check 0 list


splitOn1stSpace ::  String
                ->  (String , String)
splitOn1stSpace = splitOn1st' Data.Char.isSpace


splitOn1st_ ::  Char  ->  String
            ->  (String , String)
splitOn1st_ delim list
    | i < 0     = (list , [])
    | otherwise = (take i list , rest)
    where
    (i , rest) = spliton delim list ; spliton ':' = colon ; spliton '.' = dot ; spliton '>' = gt ; spliton '\"' = quote
    spliton '#' = sharp ; spliton '=' = eq ; spliton ',' = comma ; spliton ' ' = space ; spliton '\v' = white
    spliton _ = error$ "splitOn1st_ is hard-coded and doesn't support "++(shows delim ", fix that or use splitOn1st")
    n = (_intmin , []) ; y l = (0 , l) ; b (j,l) = (j+1,l)
    colon [] = n ; colon (':':xs) = y xs ; colon (_:xs) = b (colon xs)
    dot [] = n ; dot ('.':xs) = y xs ; dot (_:xs) = b (dot xs)
    sharp [] = n ; sharp ('#':xs) = y xs ; sharp (_:xs) = b (sharp xs)
    comma [] = n ; comma (',':xs) = y xs ; comma (_:xs) = b (comma xs)
    space [] = n ; space (' ':xs) = y xs ; space (_:xs) = b (space xs)
    quote [] = n ; quote ('\"':xs) = y xs ; quote (_:xs) = b (quote xs)
    white [] = n ; white (' ':xs) = y xs ; white ('\t':xs) = y xs ; white ('\r':xs) = y xs ; white ('\n':xs) = y xs ; white ('\v':xs) = y xs ; white ('\b':xs) = y xs ; white ('\f':xs) = y xs ; white (_:xs) = b (white xs)
    eq [] = n ; eq ('=':xs) = y xs ; eq (_:xs) = b (eq xs)
    gt [] = n ; gt ('>':xs) = y xs ; gt (_:xs) = b (gt xs)




splitUp ::  (String->String)  ->  [String]  ->  String  ->  String
        ->  Str.Pairs
splitUp     _ _ _ ""
    = []
splitUp     _ _ "" src
    = [(src,"")]
splitUp     _ [] _ src
    = [(src,"")]
splitUp     withmatch allbeginners ender src
    | nomatchpossible || null beginners     = [(src,"")]
    | otherwise                             = _splitup src
    where
    nomatchpossible = not$ Lst.isInfixOf ender src -- oddly enough this extra work does pay off perf-wise
    beginners = beginners' ~| length~.((==)beg0len)
    beginners' = allbeginners ~> forNoNilsEach reverse
    beg0len = beg0~>length
    beg0 = beginners'@!0

    lastidx = (beginners~>length == 1) |? (lastidx') |! (lastidx'')
    lastidx' revstr = lastIndexOfSub revstr beg0
    lastidx'' revstr = (beginners>~ (lastIndexOfSub revstr)) ~> maximum

    _splitup str =
        (tolist "" pre) ++ (tolist beginner (withmatch match)) ++ --  only recurse if we have a good reason:
            (nomatch && splitat==0 |? (tolist "" rest) |! (_splitup rest))
        where
        pre = str ~> (take$ nomatch |? splitat |! begpos)
        match = nomatch |? "" |! (str ~> (take endpos) ~> (drop $begpos+beg0len))
        rest = str ~> (drop$ nomatch |? splitat |! endposl)
        beginner = nomatch |? "" |! str ~> (take endpos) ~> (drop begpos) ~> (take beg0len)
        nomatch = endpos<0 || begpos<0
        splitat = (nomatch && endpos>=0) |? endposl |! 0
        endpos = indexOfSub str ender
        begpos = endpos<0 |? -1 |! lastidx$ str ~> (take endpos) ~> reverse
        endposl = endpos + (ender~>length)
        tolist beg val = (null val && null beg) |? [] |! [(val,beg)]
