module Exercises where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array (foldM, head, nub, sort, tail)
import Data.Maybe (Maybe)

third :: forall a. Array a -> Maybe a
third a = do
  twoAndFurther <- tail a
  threeAndFurther <- tail twoAndFurther
  head threeAndFurther

sums :: Array Int -> Array Int
sums = 
  foldM (\a b -> [a, a + b]) 0 
  >>> nub 
  >>> sort

-- X> 1. (Difficult) The following is a simple way to estimate pi: randomly choose a large number `N` of points in the unit square, and count the number `n` which lie in the inscribed circle. An estimate for pi is `4n/N`. Use the `RANDOM` and `ST` effects with the `forE` function to write a function which estimates pi in this way.

--TODO : purescript-random

-- rndPi :: Eff () Number
-- rndPi = do
--   random



