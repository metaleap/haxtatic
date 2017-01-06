{-# OPTIONS_GHC -Wall #-}
module Base where

import qualified Control.Monad
import qualified Data.Char
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Time.Calendar
import qualified Data.Time.Clock
import Data.Function ( (&) )
import qualified Text.Read



is ::
    (Foldable f)=>
    f a -> Bool

is = not.null




when ::
    Bool -> a -> a -> a

when True v _ = v
when False _ v = v



(~.) = flip (.)

(~:) = (&)

(~>) = (&)



(=:) = (,)
infix 0 =:

--  (=:=)  :: Eq eq => (eq, eq) -> Bool
--  (=:=) = uncurry (==)




(>~) ::
    (Functor f)=>
    f a -> (a -> b) -> f b

(>~) = flip fmap
infix 8 >~




(|~) = filter
infix 7 |~

(~|) = flip filter
infix 7 ~|




(>>~) ::
    (Traversable t, Monad m)=>
    t a -> (a -> m b) -> m (t b)

(>>~) = Control.Monad.forM
infix 8 >>~




(>>|) ::
    (Applicative m)=>
    [a] -> (a -> m Bool) -> m [a]

(>>|) = flip Control.Monad.filterM





(|?) ::
    Bool -> a -> a -> a

(|?) = when
infixl 1 |?


(|!) = ($)
infixr 0 |!


(-|=) = Data.Maybe.fromMaybe
infix 9 -|=

(=|-) = flip maybe
infix 9 =|-


(#) ::
    [a] -> Int -> a
--  alias for: `!!` ..for these most common cases, no need to `fold`
infix 9 #
[] #_ = undefined  --  rids this Careful Coder (TM) of the pesky 'non-exhaustive patterns' warning
(x:_) #0 = x
(_:x:_) #1 = x
--  (_:_:x:_) #2 = x
--  (_:_:_:x:_) #3 = x
--  (_:_:_:_:x:_) #4 = x
--  (_:_:_:_:_:x:_) #5 = x
--  (_:_:_:_:_:_:x:_) #6 = x
--  (_:_:_:_:_:_:_:x:_) #7 = x
--  (_:_:_:_:_:_:_:_:x:_) #8 = x
--  (_:_:_:_:_:_:_:_:_:x:_) #9 = x
--  these above are all branches so release only as necessary
list #i = (drop i list) #0
