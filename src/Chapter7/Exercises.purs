module Chapter7.Exercises where

import Prelude

import Chapter7.AddressBook
import Chapter7.AddressBook.Validation

import Control.Apply (lift2)
import Data.Either(Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex
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
