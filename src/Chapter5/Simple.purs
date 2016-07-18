module Chapter5.Simple where

import Prelude

euclidGCD :: Int -> Int -> Int
euclidGCD 0 n = n
euclidGCD m 0 = m
euclidGCD m n 
    | n > m     = euclidGCD (n - m) m
    | otherwise = euclidGCD n (m - n)

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * (factorial (n -1))

type Address = {street :: String, city :: String }
type Person = {name :: String, address :: Address }

sameCity :: Person -> Person ->  Boolean
sameCity {address: {street: s, city: c}} {address: {street: s', city: c'}} = 
    s == s' && c == c'
--The most general type for this function is a wrapped address, while the livesInLA function 
-- may be expressed with {address: {city: c}} such that c == "Los Angeles"

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [a] = a
fromSingleton def _ = def

