module Chapter10.Control.Monad.Eff.Storage where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Foreign (Foreign)

foreign import data STORAGE :: !

foreign import setItem :: forall eff. String -> String -> Eff (storage :: STORAGE | eff) Unit

foreign import getItem :: forall eff. String -> Eff (storage :: STORAGE | eff) Foreign

foreign import removeItem :: forall eff. String -> Eff (storage :: STORAGE | eff) Unit

