module Main where

import SimpleMath

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow, CONSOLE, log)

main = logShow (circleArea 7.0)


