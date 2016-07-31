module Chapter9.Examples.Rectangle where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (arc, CANVAS, rect, fillPath, setFillStyle, getContext2D, getCanvasElementById)
import Partial.Unsafe (unsafePartial)

main :: Eff (canvas :: CANVAS) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle "#0000FF" ctx

  fillPath ctx $ do
      a <- rect ctx
            { x: 250.0
            , y: 250.0
            , w: 50.0
            , h: 50.0 }
      b <- rect ctx
            { x: 150.0
            , y: 250.0
            , w: 50.0
            , h: 50.0 
            }
      c <- arc ctx {
            x: 50.0,
            y: 50.0,
            r: 50.0,
            start: 10.0,
            end: 11.0
            }
      pure c 
