module Chapter7.Exercises where

import Prelude
import Chapter7.AddressBook
import Chapter7.AddressBook.Validation
import Data.String.Regex
import Control.Apply (lift2, lift3)
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl, foldr, class Foldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, class Traversable)
import Data.Validation.Semigroup (V)
import Partial.Unsafe (unsafePartial)

optionalAdd :: Maybe Number -> Maybe Number -> Maybe Number
optionalAdd = optionalNumOp (+)

optonalMult :: Maybe Number -> Maybe Number -> Maybe Number
optonalMult = optionalNumOp (*)

optionalDiv :: Maybe Number -> Maybe Number -> Maybe Number
optionalDiv = optionalNumOp (/)

optionalSub :: Maybe Number -> Maybe Number -> Maybe Number
optionalSub = optionalNumOp (-)

optionalNumOp :: (Number -> Number -> Number) -> Maybe Number -> Maybe Number -> Maybe Number
optionalNumOp f = lift2 f

lift3' :: forall a b c d f. (Apply f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3' f a b c = f <$> a <*> b <*> c

combineMaybe :: forall a f. (Applicative f) => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just fa) = Just <$> fa

--Validation Exercises

stateRegex :: Regex
stateRegex = unsafePartial case regex "[a-zA-Z]{2}" noFlags of
                                Right r -> r

validateAddress' (Address o) = 
    address <$> (nonWhitespace "Street" o.street *> pure o.street)
            <*> (nonWhitespace "City"   o.city *> pure o.city)
            <*> (lengthIs "State" 2 o.state *> matches "State" stateRegex o.state *> pure o.state)

notEmptyRegex :: Regex
notEmptyRegex = unsafePartial case regex ".*\\S.*" noFlags of
                                   Right r -> r

nonWhitespace :: String -> String -> V Errors Unit
nonWhitespace field = matches field notEmptyRegex 

data Tree a = Leaf a | Branch (Tree a) a (Tree a)

instance treeFunctor :: Functor Tree where
    map f (Leaf a) = Leaf $ f a
    map f (Branch l a r) = Branch (map f l) (f a) (map f r)

instance treeFoldable :: Foldable Tree where
    foldr f z (Leaf a) = f a z
    foldr f z (Branch l a r) = foldr f (f a r') l
        where r' = foldr f z r
    
    foldl f z (Leaf a) = f z a
    foldl f z (Branch l a r) = foldl f (f (foldl f z l) a) r
    
    foldMap f (Leaf a) = f a
    foldMap f (Branch l a r) = foldMap f l <> f a <> foldMap f r

instance treeTraversable :: Traversable Tree  where
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Branch l a r) = Branch <$> traverse f l <*> f a <*> traverse f r
    sequence = traverse id


