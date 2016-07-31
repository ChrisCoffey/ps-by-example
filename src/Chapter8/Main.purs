module Chapter8.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM, random)


main :: forall e. Eff ( random :: RANDOM , console :: CONSOLE | e) Unit
main = do
    r <- random
    log <<< show $ r
