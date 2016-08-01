module Chapter9.Exercises where

import Prelude
import Math as Math
import Chapter9.DOM.Eff (addEventListener, querySelector)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Random (randomInt, random)
import Data.Array (range, (..))
import Data.Array.Partial (tail, head)
import Data.Foldable (for_)
import Data.Int (hexadecimal, toStringAs, toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Graphics.Canvas (rotate, translate, strokePath, Context2D, setStrokeStyle, CANVAS, closePath, lineTo, moveTo, fillPath, setFillStyle, arc, rect, getContext2D, getCanvasElementById)
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

pointSampleF = void $ unsafePartial do
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    setFillStyle "00FF00" ctx
    setStrokeStyle "00FF00" ctx

    let pts = unChanged <$> [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.9]
    log "rendering points"
    renderPath ctx pts
    log "Done rendering points"
   
    renderPath ctx <<< map interesting <<< map toNumber $ 1..500


circlesOnClick ctx = do 
    setStrokeStyle "#000000" ctx
    fillPath ctx $ rect ctx {x: 0.0,y: 0.0, w: 0.0, h: 0.0}
    
    for_ (1 .. 100) \_ -> do
        x <- random
        y <- random
        r <- random
        r' <- randomInt 0 255
        g <- randomInt 0 255
        b <- randomInt 0 255
        let cString = (toStringAs hexadecimal r') <>
                      (toStringAs hexadecimal g) <>
                      (toStringAs hexadecimal b)
        setFillStyle ("#"<>cString) ctx
        strokeAndFill ctx $ arc ctx
            { x     : x * 600.0
            , y     : y * 600.0
            , r     : r * 50.0
            , start : 0.0
            , end   : Math.pi * 2.0
            }

rotateAround ctx point = do
    translate {translateX: 0.0, translateY: 0.0} ctx
    rotate 1.0 ctx

main = void $ unsafePartial do
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas
    node <- querySelector "#canvas"
    for_ node $ addEventListener "click" $ void do
        circlesOnClick ctx
        rotateAround ctx {x: 0.0, y: 0.0}


strokeAndFill ctx path = do
    fillPath ctx path
    strokePath ctx path

