module Example.Rectangle where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, rect, fillPath, setFillStyle, getContext2D,
                        getCanvasElementById)
import Partial.Unsafe (unsafePartial)

main :: Eff (canvas :: CANVAS) Unit
main = void $ unsafePartial do
  -- getCanvasElementById :: forall eff. String -> Eff (canvas :: CANVAS | eff) (Maybe CanvasElement)
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  -- setFillStyle :: forall eff. String -> Context2D -> Eff (canvas :: CANVAS | eff) Context2D
  _ <- setFillStyle "#0000FF" ctx

  -- fillPath :: forall eff. Context2D -> Eff (canvas :: CANVAS | eff) Context2D -> Eff (canvas :: CANVAS | eff) Context2D
  --     rect :: forall eff.   Context2D -> Rectangle -> Eff (canvas :: CANVAS | eff) Context2D
  _ <- fillPath ctx $ rect ctx
    { x: 250.0
    , y: 250.0
    , w: 100.0
    , h: 100.0
    }

  _ <- fillPath ctx $ do
    _ <- rect ctx
      { x: 150.0
      , y: 150.0
      , w: 100.0
      , h: 100.0
      }
    rect ctx
      { x: 350.0
      , y: 350.0
      , w: 100.0
      , h: 100.0
      }

  fillPath ctx $ rect ctx { x: 150.0 , y: 150.0 , w: 100.0 , h: 100.0 } >>=
    (\_ -> rect ctx { x: 350.0 , y: 350.0 , w: 100.0 , h: 100.0 })
