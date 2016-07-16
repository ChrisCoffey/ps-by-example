module Chapter4.Simple where

import Prelude
import Control.MonadZero (guard)
import Data.Array (filter, (..), length, cons)
import Data.Array.Partial (head, tail)
import Data.Foldable (foldl, product)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = 
    if n < 0
       then isEven (n + 2)
       else isEven (n - 2)

evenCount :: Array Int -> Int
evenCount [] = 0
evenCount arr = 
    if isEven $ unsafePartial head arr
       then 1 + (evenCount $ unsafePartial tail arr)
       else evenCount $ unsafePartial tail arr

squares :: Array Number -> Array Number
squares = map (\n -> n * n)

onlyPositive :: Array Number -> Array Number
onlyPositive = (<$?>) (\n -> n >= 0.0)

infix 7 filter as <$?>

--ha. ha. ha.
factors :: Int -> Array (Array Int)
factors n = do
    i <- 1..n
    j <- i..n
    guard $ i * j == n
    pure [i,j]

isPrime :: Int -> Boolean
isPrime = (==) 1 <<< length <<< factors

cartesianProduct :: forall a b. Array a -> Array b -> Array (Tuple a b)
cartesianProduct l r = do
    a <- l
    b <- r
    pure $ Tuple a b

tripsLessThan :: Int -> Array (Array Int)
tripsLessThan n = do
    a <- 1 .. n
    b <- a .. (n -1)
    c <- b .. (n -1)
    guard $ isTrip a b c
    pure [a,b,c]
        where 
        isTrip a b c = (a*a) + (b*b) == c*c

factorizations :: Int -> Array (Array Int)
factorizations 1 = [[]]
factorizations n = do
    a <- 2 .. n
    guard $ n `mod` a == 0
    rest <- factorizations (n `div` a)
    guard $ product ([a]<>rest) == n
    pure $ [a] <> rest

--The inverse situation where the entire array is false can be checked with foldl (==) false ...
allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

count' :: forall a. (a -> Boolean) -> Array a -> Int
count' p xs = go xs 0
  where 
    go [] a    = a 
    go xs' acc = if p (unsafePartial head xs')
                    then go (unsafePartial tail xs') (acc + 1)
                    else go (unsafePartial tail xs') acc

reverse' :: forall a. Array a -> Array a
reverse' = foldl (flip cons) []


