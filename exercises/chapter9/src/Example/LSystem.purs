module Example.LSystem where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (concatMap, foldM)
import Control.Monad.Eff (Eff)
import Graphics.Canvas (CANVAS, strokePath, setStrokeStyle, lineTo, moveTo,
                        getContext2D, getCanvasElementById, closePath, setFillStyle)
import Math as Math
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
  go s m = go (concatMap prod s) (m - 1)

data Alphabet = L | R | F

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
    initial = [F, R, R, F, R, R, F, R, R]

    productions :: Alphabet -> Sentence
    productions L = [L]
    productions R = [R]
    productions F = [F, L, F, R, R, F, L, F]

    interpret :: State -> Alphabet -> Eff (canvas :: CANVAS) State
    interpret state L = pure $ state { theta = state.theta - Math.pi / 3.0 }
    interpret state R = pure $ state { theta = state.theta + Math.pi / 3.0 }
    interpret state F = do
      let x = state.x + Math.cos state.theta * 1.5
          y = state.y + Math.sin state.theta * 1.5
      _ <- moveTo ctx state.x state.y
      _ <- lineTo ctx x y
      pure { x, y, theta: state.theta }

    initialState :: State
    initialState = { x: 120.0, y: 200.0, theta: 0.0 }

  _ <- setStrokeStyle "#333333" ctx
  _ <- setFillStyle "#FF0000" ctx

  strokePath ctx $ do
    _ <- lsystem initial productions interpret 5 initialState
    closePath ctx

  -- fillPath ctx $ do
  --   _ <- pure path
  --   closePath ctx
