{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Util where

import Base

import qualified Data.Char
import qualified Data.List
import qualified Data.Ratio
import qualified Data.Set
import qualified Data.Time.Calendar
import qualified Data.Time.Clock
import qualified Text.Read



type StringPairs = [(String , String)]



_intmin = minBound::Int
dateTime0 = Data.Time.Clock.UTCTime {
                Data.Time.Clock.utctDay = Data.Time.Calendar.ModifiedJulianDay {
                                            Data.Time.Calendar.toModifiedJulianDay = 0 },
                Data.Time.Clock.utctDayTime = 0
            }



dtInts :: Data.Time.Clock.UTCTime -> [Int]
dtInts utctime =
    [ modifiedjulianday , daytimenum , daytimedenom ]
    where
    modifiedjulianday = fromInteger$ Data.Time.Calendar.toModifiedJulianDay (Data.Time.Clock.utctDay utctime)
    daytimenum = fromInteger$ Data.Ratio.numerator daytime
    daytimedenom = fromInteger$ Data.Ratio.denominator daytime
    daytime = toRational (Data.Time.Clock.utctDayTime utctime)


facF n = foldr (*) 1 [1..n]
facR 0 = 1
facR n = n * facR (n - 1)


both (fun1,fun2) (tfst,tsnd) =
    (fun1 tfst , fun2 tsnd)

both' f = both (f,f)


bothFsts fn (fst1,_) (fst2,_) =
    fn fst1 fst2

bothSnds fn (_,snd1) (_,snd2) =
    fn snd1 snd2

bothTrim = both (trim,trim)

butNot notval defval val
    |(val==notval)= defval
    |(otherwise)= val


ifNo val defval =
    if null val then defval else val

ifIs testval op =
    if null testval then [] else op testval

onlyIf val goalval defval =
    if val==goalval then val else defval

noneOf vals val =
    all (val/=) vals



repeatedly fn arg =
    let result = fn arg
    in if (result==arg) then result else repeatedly fn result

repeatWhile again fn arg =
    let result = fn arg
    in if not$ again result then result else repeatWhile again fn result


via fn =
    --  use it to `>>=` a value over to fn but then
    --  discard its return and return the value instead
    ((>>)fn).return


clamp minval maxval =
    (max minval) . (min maxval)


duration =
    flip Data.Time.Clock.diffUTCTime


-- for uses such as `crop` without (directly) taking the `length`
dropLast 0 = id
dropLast 1 = init
dropLast n = (@!n) . reverse . Data.List.inits
-- dropLast n l = l~>take (l~>length - n)


takeLast 0 = const []
takeLast 1 = (:[]).last
takeLast n = (@!n) . reverse . Data.List.tails


indexed l =
    zip [0 .. ] l

reOrdered [] l = l
reOrdered _ [] = []
reOrdered newordering l =
    newordering >~ (l@!)


shiftLeft [] = []
shiftLeft (start:rest) =
    rest ++ [start]


shuffleBasic [] list = list
shuffleBasic _ [] = []
shuffleBasic (rnd:rnds) list
    | (null left) = shuffleBasic rnds right
    | (otherwise) = (last left) : (shuffleBasic rnds ((init left) ++ right))
    where (left , right) = splitAt (rnd `mod` list~>length) list

shuffleExtra rnds@(_:_:_) list@(_:_:_) =
    let shiftby = (rnds@!1) `mod` (list~>length - 1)
        reverseordont = if 0 == (shiftby `mod` 2) then reverse else id
        shuffled = shuffleBasic rnds list
        shifted = times shiftby shiftLeft shuffled
    in reverseordont shifted
shuffleExtra rnds list =
    shuffleBasic rnds list


times 0 _ =
    id
times n func =
    (times (n - 1) func) . func


crop 0 0 = id
crop 0 1 = dropLast 1
crop 0 end = dropLast end
crop 1 0 = drop 1 -- `tail` could error out, one less worry
crop start 0 = drop start
crop start end =
    (drop start) . (dropLast end)

cropOn1st delim cropafter trimitemsafterdelim oncrop list =
    let i = indexOf delim list
    in if i < 0 then list else
        (oncrop . (trimEnd' trimitemsafterdelim) . (take (i+cropafter))) list

count item =
    countBy (==item)

countBy predicate =
    next 0 where
    next counter [] =
        counter
    next counter (this:rest)
        |(predicate this)= next (counter+1) rest
        |(otherwise)= next counter rest

countSub _ [] = 0
countSub [] _ = 0
countSub list sub =
    foldr each 0 (Data.List.tails list) where
        each listtail counter =
            if Data.List.isPrefixOf sub listtail then counter + 1 else counter

countAnySubs list subs =
    --  equivalent to, but faster than:
    --      sum (map (countSub list) subs)
    foldr each 0 (Data.List.tails list) where
    each listtail counter =
        if any isprefix subs then counter + 1 else counter where
        isprefix sub = Data.List.isPrefixOf sub listtail

countSubVsSubs list (sub,subs) =
    --  equivalent to, but faster than:
    --      (countSub list sub, countAnySubs list subs)
    foldr each (0,0) (Data.List.tails list) where
    each listtail counter =
        let isprefix sublist = Data.List.isPrefixOf sublist listtail
            isp1 = isprefix sub
            isp2 = any isprefix subs
        in if isp1 || isp2
            then let (c1,c2) = counter in
                    (if isp1 then c1 + 1 else c1,
                    if isp2 then c2 + 1 else c2)
            else counter



begins item (this:_) =
    item == this
begins _ _ =
    False

fstBegins item ((this:_),_) =
    item == this
fstBegins _ _ =
    False

contains :: (Eq t)=> [t] -> [t] -> Bool
contains = flip Data.List.isInfixOf

endsWith :: (Eq t)=> [t] -> [t] -> Bool
endsWith = flip Data.List.isSuffixOf

startsWith :: (Eq t)=> [t] -> [t] -> Bool
startsWith = flip Data.List.isPrefixOf

toLower :: String -> String
toLower = (>~ Data.Char.toLower)

toUpper :: String -> String
toUpper = (>~ Data.Char.toUpper)

join = Data.List.intercalate

lookup key defval = (defval -|=) . (Data.List.lookup key)

subAt start len =
    (take len) . (drop start)

substitute old new
    |(old==new)= id
    |(otherwise)= fmap subst where
        subst item |(item==old)= new |(otherwise)= item

substitute' olds new =
    fmap subst where
        subst item |(elem item olds)= new |(otherwise)= item

trim = trim'' Data.Char.isSpace
trim' [] = id
trim' dropitems = trim'' (`elem` dropitems)
trim'' fn = (trimStart'' fn) ~. (trimEnd'' fn)
trimSpaceOr dropitems = trim'' (\c -> Data.Char.isSpace c || elem c dropitems )

trimEnd = trimEnd'' Data.Char.isSpace
trimEnd' [] = id
trimEnd' dropitems = trimEnd'' (`elem` dropitems)
trimEnd'' = Data.List.dropWhileEnd

trimStart = trimStart'' Data.Char.isSpace
trimStart' [] = id
trimStart' dropitems = trimStart'' (`elem` dropitems)
trimStart'' = Data.List.dropWhile


tryParse nullval errval toparsestr str =
    if (null str) then nullval else
        errval -|= (Text.Read.readMaybe$ toparsestr str)

tryParseOr defval =
    (defval -|=).Text.Read.readMaybe



noMaybes [] = []
noMaybes ((Just val):more) = val:(noMaybes more)
noMaybes (_:more) = noMaybes more

noNils [] = []
noNils ([]:more) = noNils more
noNils (item:more) = item:(noNils more)

noNilFsts [] = []
noNilFsts (([],_):more) = noNilFsts more
noNilFsts (item:more) = item:(noNilFsts more)

forNoNilsEach _ [] = []
forNoNilsEach func ([]:more) = forNoNilsEach func more
forNoNilsEach func (item:more) = (func item):(forNoNilsEach func more)

keepNoNilFsts _ [] = []
-- keepNoNilFsts func (([],_):more) = keepNoNilFsts func more
keepNoNilFsts func (item:more)
    | (has f) = (f,s):(keepNoNilFsts func more)
    | (otherwise) = keepNoNilFsts func more
    where (f,s) = func item



uniqueO:: (Eq a)=> [a] -> [a]
uniqueO = Data.List.nub

uniqueU:: (Ord a)=> [a] -> [a]
-- http://stackoverflow.com/a/16109302
uniqueU = ((fmap (@!0)) . Data.List.group . Data.List.sort)

unique:: (Ord a)=> [a] -> [a]
-- http://stackoverflow.com/a/16111081
unique =
    halp Data.Set.empty
    where
    halp _ [] = []
    halp set (this:rest) =
        if Data.Set.member this set
            then halp set rest
            else this : (halp (Data.Set.insert this set) rest)

uniqueBy:: (any -> any -> Bool) -> [any] -> [any]
uniqueBy = Data.List.nubBy

uniqueFst:: (Eq eq)=> [(eq,any)] -> [(eq,any)]
uniqueFst = uniqueBy (bothFsts (==))

uniqueSnd:: (Eq eq)=> [(any,eq)] -> [(any,eq)]
uniqueSnd = uniqueBy (bothSnds (==))



mergeDuplFsts isduplicate mergevalues =
    mergeduplfsts where
    mergeduplfsts [] = []
    mergeduplfsts ((dispair@(disname,disval)):morepairs) =
        let (duplpairs , otherpairs) = morepairs ~> (Data.List.partition $(isduplicate disname).fst)
        in if null duplpairs
            then dispair : (mergeduplfsts morepairs)
            else (disname , mergevalues$ disval:(duplpairs>~snd)) : (mergeduplfsts otherpairs)



lengthGEq 0 = const True
lengthGEq n = has . drop (n - 1)

lengthGt 0 = has
lengthGt n = has . drop n

excerpt maxlen str =
    if s==str then s else (s++"...")
    where s = take maxlen str

fuseElems is2fuse fusion (this:next:more) =
    (fused:rest) where
        nofuse = not$ is2fuse this next
        fused = nofuse |? this |! fusion this next
        rest = fuseElems is2fuse fusion$
                nofuse |? (next:more) |! more
fuseElems _ _ l = l



indexOf _ [] =
    _intmin
indexOf item (this:rest) =
    if this==item then 0 else
        (1 + (indexOf item rest))



_indexof_droptil delim = _indexof_droptil' (delim==)
_indexof_droptil' _ _ [] =
    (_intmin , [])
_indexof_droptil' predicate counter list@(this:rest) =
    if (predicate this) then (counter , list)
        else _indexof_droptil' predicate (counter + 1) rest


indexOf1st items list =
    i where (i , _) = _indexof_droptil' (`elem` items) 0 list

indexOfSub [] _ =
    _intmin
indexOfSub _ [] =
    _intmin
indexOfSub haystack needle =
    let (startindex , _haystack) = _indexof_droptil (needle@!0) 0 haystack
    in if startindex<0 then startindex else
        startindex + indexofsub _haystack needle
        where
        indexofsub [] _ =
            _intmin
        indexofsub list@(_:rest) sub =
            if Data.List.isPrefixOf sub list then 0
                else (1 + indexofsub rest sub)
--  indexOfSub list@(_:rest) sub =
--      if Data.List.isPrefixOf sub list then 0
--          else (1 + indexOfSub rest sub)


indexOfSubs1st [] _ =
    (_intmin , [] )
indexOfSubs1st _ [] =
    (_intmin , [] )
indexOfSubs1st subs str =
    let startchars = unique$ subs>~(@!0)
        (startindex , _haystack) = _indexof_droptil' (`elem` startchars) 0 str
        iidxs = isubs >~ (both (id,indexof)) ~|(>=0).snd
        isubs = indexed subs
        indexof = indexOfSub _haystack
        (i,index) = Data.List.minimumBy (bothSnds compare) iidxs
    in if startindex<0 || null iidxs || index<0
        then ( _intmin , [] )
        else ( index+startindex , subs@!i )



lastIndexOfSub revstr revsub
    |(idx<0)= idx
    |(otherwise)= revstr~>length - revsub~>length - idx
    where
        idx = indexOfSub revstr revsub



replaceSub ([] , _) = id
replaceSub (old , new) =
    replsub where
    replsub [] = []
    replsub list@(this : more)
        | (isprefix list) = withnew list
        | (otherwise) = this : replsub more
    withnew = ((new ++) . replsub . dropold)
    dropold = drop $old~>length
    isprefix = Data.List.isPrefixOf old


replaceSub' ([] , _) = id
replaceSub' (old , new) =
    replsub where
    replsub [] = []
    replsub str =
        if str==old then new
        else replrec (indexOfSub str old , old , oldlen) str
    replrec = _replcore replsub (const new)
    oldlen = old~>length


replaceSubsFew [] = id
replaceSubsFew [replpair] = replaceSub replpair
replaceSubsFew replpairs =
    replall
    where
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


replaceSubsMany [] list = list
replaceSubsMany [replpair] list = replaceSub replpair list
replaceSubsMany replpairs list =
    Data.List.foldl' replsub list replpairs
    where
    replsub = flip replaceSub --- wtf foldl'..


replaceSubsBy [] _ = id
replaceSubsBy olds tonew =
    replby
    where
    replby [] = []
    replby str =
        let (idx , old) = indexOfSubs1st olds str
        in replrec (idx , old , old~>length) str
    replrec = _replcore replby tonew


_replcore recurse tonew (idx , old , oldlen) str =
    if idx<0 then str else
        let pre = take idx str
            rest = drop (idx + oldlen) str
        in pre ++ tonew old ++ recurse rest



splitOn delim =
    splitOn' (delim==)
splitOn' predicate =
    foldr each [[]] where
        each _ [] = []
        each item accum@(item0:rest)
            |(predicate item)= []:accum
            |(otherwise)= (item:item0):rest

splitOn1st delim =
    splitOn1st' (delim==)
splitOn1st' predicate list =
    let (i , rest) = _indexof_droptil' predicate 0 list
    in if i<0 then (list , [])
        else (take i list , drop 1 rest)
splitOn1stSpace = splitOn1st' Data.Char.isSpace

splitOn1st_ delim list
    |(i < 0)= (list , [])
    |(otherwise) = (take i list , rest)
    where
    (i , rest) = spliton delim list ; spliton ':' = colon ; spliton '>' = gt ; spliton '\"' = quote
    spliton '#' = sharp ; spliton '=' = eq ; spliton ',' = comma ; spliton ' ' = space ; spliton '\v' = white
    spliton _ = error ("splitOn1st_ is hard-coded and doesn't support "++(show delim) ++ ", fix that or use splitOn1st")
    n = (_intmin , []) ; y l = (0 , l) ; b (j,l) = (j+1,l)
    colon [] = n ; colon (':':xs) = y xs ; colon (_:xs) = b (colon xs)
    sharp [] = n ; sharp ('#':xs) = y xs ; sharp (_:xs) = b (sharp xs)
    comma [] = n ; comma (',':xs) = y xs ; comma (_:xs) = b (comma xs)
    space [] = n ; space (' ':xs) = y xs ; space (_:xs) = b (space xs)
    quote [] = n ; quote ('\"':xs) = y xs ; quote (_:xs) = b (quote xs)
    white [] = n ; white (' ':xs) = y xs ; white ('\t':xs) = y xs ; white ('\r':xs) = y xs ; white ('\n':xs) = y xs ; white ('\v':xs) = y xs ; white ('\b':xs) = y xs ; white ('\f':xs) = y xs ; white (_:xs) = b (white xs)
    eq [] = n ; eq ('=':xs) = y xs ; eq (_:xs) = b (eq xs)
    gt [] = n ; gt ('>':xs) = y xs ; gt (_:xs) = b (gt xs)


splitIt i list
    |(i < 0)= (list , [])
    |(otherwise)= (take i list, drop (i+1) list)


splitUp _ _ _ "" = []
splitUp _ _ "" src = [(src,"")]
splitUp _ [] _ src = [(src,"")]
splitUp withmatch allbeginners end src =
    if nomatchpossible || null beginners
        then [(src,"")]
        else _splitup src
    where
    nomatchpossible = not$ Data.List.isInfixOf end src -- oddly enough this extra work does pay off perf-wise
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
        endpos = indexOfSub str end
        begpos = endpos<0 |? -1 |! lastidx$ str ~> (take endpos) ~> reverse
        endposl = endpos + (end~>length)
        tolist beg val = (null val && null beg) |? [] |! [(val,beg)]
