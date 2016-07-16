module Chapter4.FileSystem where

import Prelude
import Chapter4.Path
import Control.MonadZero (guard)
import Data.Array (filter, (:))
import Data.Array.Partial (head, tail)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

allFiles :: Path -> Array Path
allFiles path = path : do
    child <- ls path
    allFiles child

onlyFiles :: Path -> Array Path
onlyFiles = filter (not <<< isDirectory) <<< allFiles

maxMin :: Path -> Tuple Path Path
maxMin = paired <<< onlyFiles
    where
        paired xs = Tuple (maxF xs) (minF xs)
        maxF xs = foldl (choose (>)) (unsafePartial head xs) xs
        minF xs = foldl (choose (<)) (unsafePartial head xs) xs
        choose f l r = 
            case size l of
                 Nothing   -> r  --Should be impossible
                 (Just ls) -> 
                    case size r of
                         Nothing   -> l --Should be impossible case
                         (Just rs) ->
                            if ls `f` rs then l else r

whereIs :: String -> Maybe Path
whereIs term = let 
    match = do
        dir <- allDirs
        matching <- dirMatch dir
        pure matching
    in case match of
        ([dir]) -> Just dir
        _       -> Nothing
    where
        allDirs = filter isDirectory $ allFiles root
        dirMatch dir = 
            do child <- ls dir
               guard $ (filename child) == (filename dir <> term)
               pure dir
