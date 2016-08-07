module Chapter9.Examples.LSystems where


import Prelude
import Math as Math
import Control.Monad.Eff (Eff)
import Data.Array (concatMap, foldM)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (closePath, fillPath, CANVAS, strokePath, setStrokeStyle, lineTo, moveTo, getContext2D, getCanvasElementById)
import Partial.Unsafe (unsafePartial)

lsystem :: forall a m s. Monad m =>
                         Array a ->
                         (a -> Array a) ->
                         (s -> a -> m s) ->
                         Int ->
                         s -> m s
lsystem init prod interpret n state = go init n
  where
  go s 0 = foldM interpret state s
  go s n = go (concatMap prod s) (n - 1)

data Alphabet = L | R | F | M

type Sentence = Array Alphabet

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

main :: Eff (canvas :: CANVAS) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  let
    initial :: Sentence
    initial = [M]

    productions :: Alphabet -> Sentence
    productions L = [L]
    productions R = [R]
    productions F = [F, L, M, L, F, R, M, R, F, R, M, R, F, L, M, L, F]
    productions M = [M, R, F, R, M, L, F, L, M, L, F, L, M, R, F, R, M]

    interpret :: State -> Alphabet -> Eff (canvas :: CANVAS) State
    interpret state L = pure $ state { theta = state.theta - Math.pi / 3.0 }
    interpret state R = pure $ state { theta = state.theta + Math.pi / 3.0 }
    interpret state _ = do
      let x = state.x + Math.cos state.theta * 1.5
          y = state.y + Math.sin state.theta * 1.5
      lineTo ctx x y
      pure { x, y, theta: state.theta }

    initialState :: State
    initialState = { x: 120.0, y: 200.0, theta: 0.4 }

  setStrokeStyle "#000000" ctx

  moveTo ctx initialState.x initialState.y
  fillPath ctx $ lsystem initial productions interpret 4 initialState
  closePath ctx
