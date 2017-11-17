module Main where

import Prelude

gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 n = n
gcd n m | n > m     = gcd (n - m) m
        | otherwise = gcd n (m - n)


factorial :: Int -> Int
factorial 0 = 0
factorial 1 = 1
factorial n = n * factorial n - 1

type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

p1 :: Person
p1 = { name: "Oliver", address: { street: "Zevenwouden", city: "Utrecht"}}

p2 :: Person
p2 = { name: "Maaike", address: { street: "Zevenwouden", city: "Utrecht"}}

sameStreet p q = p.address.street == q.address.street

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [a] = a
fromSingleton x _ = x


