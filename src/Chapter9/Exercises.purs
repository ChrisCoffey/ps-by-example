module Chapter9.Exercises where

import Prelude
import Math as Math
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Data.Array (range, (..))
import Data.Array.Partial (tail, head)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Graphics.Canvas (strokePath, Context2D, setStrokeStyle, CANVAS, closePath, lineTo, moveTo, fillPath, setFillStyle, arc, rect, getContext2D, getCanvasElementById)
import Math (sin, cos)
import Partial.Unsafe (unsafePartial)

type Point = {x:: Number, y:: Number}

renderPath :: forall e. Context2D -> Array Point ->  Eff (canvas:: CANVAS | e) Context2D
renderPath ctx pts = fillPath ctx $ do
    setStrokeStyle "#000000" ctx
    strokePath ctx $ do
        moveTo ctx h.x h.y
        traverse (\p -> lineTo ctx p.x p.y) t
        closePath ctx
    where h = unsafePartial head pts
          t = unsafePartial tail pts

unChanged n = {x: n * 100.0, y: n * 25.0}
interesting n = {x: n + 4.0, y: 150.0 + (100.0 * sin n)}

main = void $ unsafePartial do
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    setFillStyle "00FF00" ctx
    setStrokeStyle "00FF00" ctx

    let pts = unChanged <$> [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.9]
    log "rendering points"
    renderPath ctx pts
    log "Done rendering points"
   
    renderPath ctx <<< map interesting <<< map toNumber $ 1..500

