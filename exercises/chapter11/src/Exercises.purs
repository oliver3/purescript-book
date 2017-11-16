module Exercises where

import Prelude

import Control.Monad.State (State, execState, get)
import Control.Monad.State.Class (modify)
import Data.Foldable (traverse_)
import Data.String (toCharArray)

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

-- type Level = Int
-- type Doc = Reader Level String

-- line :: String -> Doc
-- line s = do
--   _ <- ask
--   pure s

-- indent :: Doc -> Doc
-- indent = local (_ + 2)

-- cat :: Array Doc -> Doc
-- cat a = sequence a