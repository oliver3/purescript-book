module Main where

import Prelude
import Data.Array (length, nubBy)
import Data.Char (toCharCode)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Function (on)
import Data.Monoid (class Monoid)
import Data.String (toCharArray)

newtype Complex = Complex
   { real :: Number
   , imaginary :: Number
   }

instance showComplex :: Show Complex where
  show (Complex {real, imaginary}) = "( " <> show real <> " + " <> show imaginary <> "i )"

instance eqComplex :: Eq Complex where
  eq (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2}) = r1 == r2 && i1 == i2

c1 :: Complex
c1 =  Complex {real: 3.0, imaginary: 4.0}
c2 :: Complex
c2 =  Complex {real: 3.0, imaginary: 4.0}
c3 :: Complex
c3 =  Complex {real: 4.0, imaginary: 5.0}

data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty a as) = show a <> show as

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty a as) (NonEmpty b bs) = a == b && as == bs

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty a as) (NonEmpty b bs) = (NonEmpty a (as <> [b] <> bs))

instance functorNonEmpty :: Functor (NonEmpty) where
  map f (NonEmpty a as) = NonEmpty (f a) (map f as)

instance foldableNonEmpty :: Foldable (NonEmpty) where
  -- foldr :: forall a r. (a -> r -> r) -> r -> f a -> r
  foldr f r (NonEmpty a as) = f a (foldr f r as)
  -- foldl :: forall a r. (r -> a -> r) -> r -> f a -> r
  foldl f r (NonEmpty a as) = foldl f (f r a) as
  -- foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
  foldMap f (NonEmpty a as) = f a <> foldMap f as


n1 :: NonEmpty Int
n1 = NonEmpty 1 [2, 3, 4]

n2 :: NonEmpty Int
n2 = NonEmpty 5 [6, 7, 8]

n12 :: NonEmpty Int
n12 = n1 <> n2

fromRight :: forall f a. Foldable f => Show a => f a -> String
fromRight = foldr (\n r -> (show n) <> r) "*"
fromLeft :: forall f a. Foldable f => Show a => f a -> String
fromLeft = foldl (\r n -> r <> (show n)) "*"

r1 = fromRight [1,2,3,4]
l1 = fromLeft [1,2,3,4]
m1 = foldMap show [1,2,3,4]

r2 = fromRight n1
l2 = fromLeft n1
m2 = foldMap show n1

data Extended a = Finite a | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
  eq (Finite a) (Finite b) = eq a b
  eq Infinite Infinite = true
  eq _ _ = false

instance ordExtended :: Ord a => Ord (Extended a) where
  compare (Finite a) (Finite b) = compare a b
  compare (Finite a) Infinite = LT
  compare Infinite (Finite b) = GT
  compare Infinite Infinite = EQ

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  -- foldr :: forall a r. (a -> r -> r) -> r -> f a -> r
  foldr f r (OneMore a fa) = f a (foldr f r fa)
  -- foldl :: forall a r. (r -> a -> r) -> r -> f a -> r
  foldl f r (OneMore a fa) = foldl f (f r a) fa
  -- foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
  foldMap f (OneMore a fa) = f a <> foldMap f fa

--------------------------------------------------

newtype HashCode = HashCode Int

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65535)

instance showHashCode :: Show HashCode where
  show (HashCode h) = "(HashCode " <> show h <> ")"

instance eqHashCode :: Eq HashCode where
  eq (HashCode h1) (HashCode h2) = eq h1 h2

class Eq a <= Hashable a where
  hash :: a -> HashCode

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

hashEqual :: forall a. Hashable a => a -> a -> Boolean
hashEqual = eq `on` hash

instance hashInt :: Hashable Int where
  hash = hashCode

instance hashBoolean :: Hashable Boolean where
  hash false = hashCode 0
  hash true  = hashCode 1

instance hashChar :: Hashable Char where
  hash = hash <<< toCharCode

instance hashArray :: Hashable a => Hashable (Array a) where
  hash = foldl combineHashes (hashCode 0) <<< map hash

instance hashString :: Hashable String where
  hash = hash <<< toCharArray

arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates arr = length arr /= length (nubHash arr)
  where nubHash = nubBy (\a b -> hashEqual a b && a == b)

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashHour :: Hashable Hour where
  hash (Hour n) = hash (mod n 12)