module Example.Random where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Data.Array (foldM, nub, sort, (..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, Context2D, arc, fillPath, getCanvasElementById, getContext2D, setFillStyle, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)

strokeFillPath :: forall eff a. Context2D -> Eff (canvas :: CANVAS | eff) a -> Eff (canvas :: CANVAS | eff) Unit
strokeFillPath ctx path = do
  _ <- strokePath ctx path
  _ <- fillPath ctx path
  pure unit

setFillStyle' :: forall eff. String -> Context2D -> Eff (canvas :: CANVAS | eff) Unit
setFillStyle' style ctx = void $ setFillStyle style ctx

setStrokeStyle' :: forall eff. String -> Context2D -> Eff (canvas :: CANVAS | eff) Unit
setStrokeStyle' style ctx = void $ setStrokeStyle style ctx

main :: Eff (canvas :: CANVAS, random :: RANDOM) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle' "#FF0000" ctx
  setStrokeStyle' "#0000FF" ctx

  for_ (1 .. 100) \_ -> do
    x <- random
    y <- random
    r <- random

    let path = arc ctx
         { x     : x * 600.0
         , y     : y * 600.0
         , r     : r * 50.0
         , start : 0.0
         , end   : Math.pi * 2.0
         }

    strokeFillPath ctx path
