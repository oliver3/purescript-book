module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.AddressBook (Entry, emptyBook, insertEntry, findEntry, showEntry)
import Data.Maybe (Maybe(Nothing, Just))
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

example :: Entry
example =
  { firstName: "John"
  , lastName: "Smith"
  , address: { street: "123 Fake St."
             , city: "Faketown"
             , state: "CA"
             }
  }

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  it "should find nothing" do
    let entry = findEntry "John" "Smith" emptyBook 
    (showEntry <$> entry) `shouldEqual` Nothing

  it "should find entry" do
    let book = insertEntry example emptyBook
    let entry = findEntry "John" "Smith" book
    (showEntry <$> entry) `shouldEqual` (Just "Smith, John: 123 Fake St., Faketown, CA")
    