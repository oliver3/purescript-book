module Example.Points where

import Prelude

import Control.Monad.Eff (Eff, foreachE)
import Data.Array ((..))
import Data.Functor ((<$>))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, Context2D, fillPath, getCanvasElementById, getContext2D, lineTo, rect, setFillStyle, setStrokeStyle, strokePath)
import Math (pi, sin)
import Partial.Unsafe (unsafePartial)

type Point = { x :: Number, y :: Number }

renderPath
   :: forall eff
    . Context2D
   -> Array Point
   -> Eff (canvas :: CANVAS | eff) Unit
renderPath ctx pts = strokePath ctx $ do
  foreachE pts drawPoint
  where
    drawPoint :: Point -> Eff (canvas :: CANVAS | eff) Unit
    drawPoint {x, y} = do
      _ <- lineTo ctx x y
      pure unit

points :: Array Point
points =
  [ {x: 50.0, y: 50.0}
  , {x: 110.0, y: 110.0}
  , {x: 130.0, y: 110.0}
  , {x: 130.0, y: 150.0}
  ]

-- 600x600
f :: Number -> Number
f x = sin (2.0 * pi * x)

plot :: (Number -> Number) -> Array Point
plot f' = (-300 .. 300) <#> toNumber
  <#> (\x -> {x: x + 300.0, y: f (x / 300.0) * -300.0 + 300.0})

main :: Eff (canvas :: CANVAS) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  _ <- setStrokeStyle "#0000FF" ctx
  _ <- strokePath ctx $ renderPath ctx (plot f)

  _ <- setStrokeStyle "#000" ctx
  _ <- strokePath ctx $ renderPath ctx [{x: 300.0, y: 0.0}, {x: 300.0, y: 600.0}]
  strokePath ctx $ renderPath ctx [{x: 0.0, y: 300.0}, {x: 600.0, y: 300.0}]

