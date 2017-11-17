module Main where

import Prelude
import Control.MonadZero (guard)
import Data.Foldable (foldl)
import Data.Array (null, filter, length, (..))
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)



isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven x = isEven (x - 2)

countEven :: Array Int -> Int
countEven a =
  if null a
    then 0
    else headEven + countEven (unsafePartial tail a)
  where
    headEven =
      if isEven (unsafePartial head a)
        then 1
        else 0

squares :: Array Int -> Array Int
squares = map (\x -> x * x)

countEven' :: Array Int -> Int
countEven' = filter isEven >>> length

carthesian :: forall x. Array x -> Array x -> Array (Array x)
carthesian as bs = do
  a <- as
  b <- bs
  pure [a,b]

pyth :: Int -> Array (Array Int)
pyth n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [a, b, c]


allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

count :: forall a. (a -> Boolean) -> Array a -> Int
count _ [] = 0
count p xs = if p (unsafePartial head xs)
                then count p (unsafePartial tail xs) + 1
                else count p (unsafePartial tail xs)


count' :: forall a. (a -> Boolean) -> Array a -> Int
count' p = foldl (\r a -> if p a then r + 1 else r) 0
