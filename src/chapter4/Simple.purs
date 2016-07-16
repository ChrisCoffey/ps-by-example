module Chapter4.Simple where

import Prelude
import Data.Array         (filter)  
import Data.Array.Partial (head, tail)
import Partial.Unsafe     (unsafePartial)

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
