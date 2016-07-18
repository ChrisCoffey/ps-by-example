module Chapter6.Hashable where

import Prelude

import Data.Char (toCharCode)
import Data.Array
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr, foldMap, class Foldable)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.String (toCharArray)
import Data.Tuple (Tuple(..))


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

instance Action m a => Action m (Array a) where
    act m arr = 
