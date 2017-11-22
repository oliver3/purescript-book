module Exercises where

import Control.Alternative
import Prelude

import Control.Monad.Except (ExceptT(..), lift, runExceptT, throwError)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (State, StateT(..), execState, get, put, runStateT)
import Control.Monad.State.Class (modify)
import Control.Monad.Writer (Writer, WriterT(..), runWriterT, tell)
import Data.Either (Either)
import Data.Foldable (traverse_)
import Data.Identity (Identity(..))
import Data.Int (even)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), charAt, drop, joinWith, stripPrefix, take, toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

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

-- Transformers

safeDivide :: Number -> Number -> ExceptT String Identity Number
safeDivide x 0.0 = throwError "#DIV/0!"
safeDivide x y = pure (x / y)

type Errors = Array String
type Log = Array String
type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

runParser :: forall a. Parser a -> String -> Either Errors (Tuple (Tuple a String) Log)
runParser p s = unwrap $ runExceptT $ runWriterT $ runStateT p s

split :: Parser String
split = do
  s <- get
  lift $ tell ["The state is " <> s]
  case s of
    "" -> throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)

string :: String -> Parser String
string prefix = do
  s <- get
  tell ["The state is " <> s]
  case s of
    "" -> throwError ["Empty string"]
    _ -> do
      case stripPrefix (Pattern prefix) s of
        Just remainder -> do
          put remainder
          pure prefix
        Nothing ->
          throwError ["String does not start with prefix"]

char :: Char -> Parser String
char c = do
  s <- get
  tell ["The state is " <> s]
  if charAt 0 s == Just c
    then do
      put (drop 1 s)
      pure (take 1 s)
    else
      throwError ["String does not start with char"]
