module Chapter6.Hashable where

import Prelude
import Data.Array
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (maximum, foldl, foldr, foldMap, class Foldable)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.String (toCharArray)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)


newtype Complex = Complex {real :: Number, imaginary :: Number}

instance cShow :: Show Complex where
    show (Complex {real, imaginary}) = "Real: " <> show real <> ", Imaginary: " <> show imaginary

instance cEq :: Eq Complex where
    eq (Complex l) (Complex r) = l.real == r.real && l.imaginary == r.imaginary


data NonEmpty a = NonEmpty a (Array a)

instance neSemi :: Semigroup (NonEmpty a) where
    append (NonEmpty a arr) (NonEmpty b arr') = NonEmpty a (append [b] (append arr  arr'))

instance neFunc :: Functor NonEmpty where
    map f (NonEmpty a arr) = NonEmpty (f a) (map f arr)

instance neFold :: Foldable NonEmpty where
   foldr f acc (NonEmpty a arr) = foldr f acc (cons a arr)
   foldl f acc (NonEmpty a arr) = foldl f acc (cons a arr)
   foldMap f (NonEmpty a arr) = append (f a) (foldMap f arr)

instance neEq :: (Eq a) => Eq (NonEmpty a) where
    eq (NonEmpty a arr) (NonEmpty a' arr') = a == a' && arr == arr'


data Extended a = Finite a | Infinite

instance extEq :: (Eq a) => Eq (Extended a) where
    eq Infinite Infinite = true
    eq (Finite a) (Finite b) = a `eq` b
    eq _ _ = false

instance extOrd :: (Ord a) => Ord (Extended a) where
    compare Infinite Infinite = EQ
    compare Infinite _        = GT
    compare _        Infinite = LT
    compare (Finite a) (Finite b) = compare a b

data OneMore f a = OneMore a (f a)

instance oneMoreFoldable :: Foldable f => Foldable (OneMore f) where
    foldr f acc (OneMore a rest) = f a (foldr f acc rest)
    foldl f acc (OneMore a rest) = foldl f (f acc a) rest
    foldMap f (OneMore a rest) = append (f a) (foldMap f rest)


class Monoid m <= Action m a where
    act :: m -> a -> a

--The Action laws should treat mEmpty as not modifying the element at all.

instance arAction :: Action m a => Action m (Array a) where
    act m as = map (act m) as

newtype Self m = Self m

instance selfAppend :: Action m a => Action m (Self m) where
    act m (Self x) = Self (m <> x)

maxOfArray :: Partial => Array Int -> Int
maxOfArray arr = unsafePartial dirtyMax arr
    where 
        dirtyMax as = case maximum as of
                           Nothing -> 0
                           Just m -> m

--- The hashing library


newtype HashCode = HashCode Int
instance hashEq :: Eq HashCode where
    eq (HashCode a) (HashCode a') = eq a a'

hashcode :: Int -> HashCode
hashcode h = HashCode (h `mod` 65535)

--Assumes that if a==b then hash(a)==hash(b)
class Eq a <= Hashable a where
    hash :: a -> HashCode

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode a) (HashCode b) = hashcode (73 * a + 51 * b)

equalHash :: forall a. Hashable a => a -> a -> Boolean
equalHash = eq `on` hash

instance hashInt :: Hashable Int where
    hash = hashcode

instance hashBool :: Hashable Boolean where
    hash false = hashcode 0
    hash true  = hashcode 1

instance hashChar :: Hashable Char where
    hash = hashcode <<< toCharCode

instance hashArray :: Hashable a => Hashable (Array a) where
    hash = foldl combineHashes (HashCode 0) <<< map hash

instance hashString :: Hashable String where
    hash = hash <<< toCharArray

elem :: forall a. Eq a =>  a -> Array a -> Boolean
elem a arr = case elemIndex a arr of
                Nothing -> false
                _       -> true

hashEqual :: forall a. Hashable a => Array a -> Array a -> Boolean
hashEqual l r = 
    if null <<< nubBy equalHash $ l' <> r'
        then false
        else foldl (\b a -> b || elem a r) false l
    where
        l' = nubBy equalHash l
        r' = nubBy equalHash r

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
    eq (Hour n) (Hour m) = mod n 12 == mod m 12

--Only trick is to make sure we keep the modular arithmetic
instance hashableHour :: Hashable Hour where
    hash (Hour n) = hash (mod n 12)


