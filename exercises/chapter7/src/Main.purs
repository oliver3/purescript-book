module Main where

import Prelude
import Control.Apply (lift2)
import Control.Bind (class Applicative)
import Data.List (List(..))
import Data.Maybe (Maybe(..))

maybeAdd :: forall t1 t2. Apply t1 => Semiring t2 => t1 t2 -> t1 t2 -> t1 t2
maybeAdd = lift2 (+)

infixl 6 maybeAdd as +?

-- Write a function combineMaybe which has type forall a f. Applicative f => Maybe (f a) -> f (Maybe a).
-- This function takes an optional computation with side-effects, and returns a side-effecting computation which has an optional result.


combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just a) = Just <$> a

-- Write a Traversable instance for the following binary tree data structure, which combines side-effects from left-to-right:
data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance showTree :: Show a => Show (Tree a) where
  show Leaf = "(Leaf)"
  show (Branch l a r) = show l <> show a <> show r

validTree :: Tree String
validTree = Branch Leaf "middle" (Branch Leaf "right" Leaf)

invalidTree :: Tree String
invalidTree = Branch Leaf "middle" (Branch Leaf "" Leaf)

traverse :: forall a b f. Applicative f => (a -> f b) -> Tree a -> f (Tree b)
traverse _ Leaf = pure Leaf
traverse f (Branch left a right) = Branch <$> traverse f left <*> f a <*> traverse f right

-- -- traverse :: forall a b f. Applicative f => (a -> f b) -> List a -> f (List b)
-- traverse _ Nil = pure Nil
-- traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

-- class (Functor t, Foldable t) <= Traversable t where
--   traverse :: forall a b f. Applicative f => (a -> f b) -> t a -> f (t b)
--   sequence :: forall a f.   Applicative f =>           t (f a) -> f (t a)

sequence :: forall a f. Applicative f => Tree (f a) -> f (Tree a)
sequence = traverse id

