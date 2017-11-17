module Exercises1 where

import Prelude

import Control.Extend (class Functor)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.ST (modifySTRef, newSTRef, readSTRef, runST)
import Data.Array (foldM, head, nub, sort, tail)
import Data.Foldable (foldl)
import Data.Maybe (Maybe)
import Math (round, sqrt)

-- | Get the first element in an array, or `Nothing` if the array is empty
-- |
-- | Running time: `O(1)`.
-- head :: forall a. Array a -> Maybe a

-- | Get all but the first element of an array, creating a new array, or
-- | `Nothing` if the array is empty
-- |
-- | Running time: `O(n)` where `n` is the length of the array
-- tail :: forall a. Array a -> Maybe (Array a)

third :: forall a. Array a -> Maybe a
third a = do
  fromSecond <- tail a
  fromThird <- tail fromSecond
  head fromThird

-- [] = []
-- [1] = [1]
-- [1, 2] = [1, 2, 3]
-- foldM :: (Int -> Int -> Array Int) -> Int -> Array Int -> Array Int


sums :: Array Int -> Array Int
sums = sort <<< nub <<< foldM (\a b -> [a, a + b]) 0

tst = sums [1, 2, 10]

-- safeDivide :: Int -> Int -> Maybe Int
-- safeDivide _ 0 = Nothing
-- safeDivide a b = Just (a / b)

safeDivide :: forall eff. Int -> Int -> Eff (exception :: EXCEPTION | eff) Int
safeDivide _ 0 = throwException (error "DIV/0")
safeDivide a b = pure (a / b)

-- The following is a simple way to estimate pi: randomly choose a large number N of points in the unit square,
-- and count the number n which lie in the inscribed circle. An estimate for pi is 4n/N.
-- Use the RANDOM and ST effects with the forE function to write a function which estimates pi in this way.

pi :: Eff (random :: RANDOM) Number
pi = runST do
  ref <- newSTRef 0.0
  forE 0 100000 \_ -> do
    x <- random
    y <- random
    _ <- modifySTRef ref (increaseIfInCircle {x,y})
    pure unit
  n <- readSTRef ref
  pure (4.0 * n / 100000.0)

increaseIfInCircle :: forall r. {x :: Number, y :: Number | r} -> Number -> Number
increaseIfInCircle p n | fromCentre p < 0.5 = n + 1.0
increaseIfInCircle _ n = n

fromCentre :: forall r. {x :: Number, y :: Number | r} -> Number
fromCentre {x, y} = sqrt (x' * x' + y' * y')
  where
    x' = x - 0.5
    y' = y - 0.5


