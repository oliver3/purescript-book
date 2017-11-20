module Exercises where

import Prelude

import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (State, execState, get)
import Control.Monad.State.Class (modify)
import Control.Monad.Writer (Writer, tell)
import Data.Foldable (traverse_)
import Data.Int (even)
import Data.Monoid.Additive (Additive(..))
import Data.String (joinWith, toCharArray)
import Data.Traversable (sequence)

sumArray :: Array Int -> State Int String
sumArray a = do
--   traverse_ (\n -> modify \sum -> sum + n) a
--   traverse_ (\n -> modify ((+) n)) a
  traverse_ (\n -> modify ((+) n)) a

--   val <- get
--   pure (show val)

--   get >>= \val -> pure (show val)

--   (\val -> show val) `map` get

--   show <$> get

  get <#> show

balanseParens :: String -> State Int Unit
balanseParens = toCharArray >>> traverse_ (case _ of
  '(' -> modify (_ + 1)
  ')' -> modify (_ - 1)
  _ -> pure unit)

testParens :: String -> Boolean
testParens s = (execState (balanseParens s) 0) == 0

-- Reader

type Level = Int
type Doc = Reader Level String

line :: String -> Doc
line s = do
  level <- ask
  pure (leftpad level s)
  where
    leftpad :: Int -> String -> String
    leftpad 0 s = s
    leftpad i s = leftpad (i - 1) (" " <> s)

indent :: Doc -> Doc
indent = local (add 2)

cat :: Array Doc -> Doc
cat = sequence >>> map (joinWith "\n")

render :: Doc -> String
render doc = runReader doc 0

-- Writer
sumArray' :: Array Int -> Writer (Additive Int) Unit
sumArray' = traverse_ (\n -> tell (Additive n))

collatz :: Int -> Int
collatz n = go 0 n
  where
    go i 1 = i
    go i n = go (i + 1) (if even n then n / 2 else 3 * n + 1)

collatz' :: Int -> Writer (Array Int) Int
collatz' n = go 0 n
  where
    go i n = do
      tell [n]
      if n == 1
        then pure i
        else go (i + 1) (if even n then n / 2 else 3 * n + 1)

