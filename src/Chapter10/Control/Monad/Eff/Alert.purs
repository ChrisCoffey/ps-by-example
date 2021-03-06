
module Chapter10.Control.Monad.Eff.Alert where

import Prelude

import Control.Monad.Eff (Eff)

foreign import data ALERT :: !

foreign import alert :: forall eff. String -> Eff (alert :: ALERT | eff) Unit

foreign import confirm :: forall eff. String -> Eff (alert:: ALERT | eff) Boolean
