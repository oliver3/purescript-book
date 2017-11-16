module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Exercises (sums, third)
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  it "should return Nothing for third []" do
    (third ([] :: Array Int)) `shouldEqual` Nothing
    
  it "should return Nothing for third [1]" do
    (third [1]) `shouldEqual` Nothing

  it "should return Nothing for third [1, 2]" do
    (third [1, 2]) `shouldEqual` Nothing

  it "should return Nothing for third [1, 2, 3]" do
    (third [1, 2, 3]) `shouldEqual` Just 3

  it "Should return [0] for sums []" do
    (sums []) `shouldEqual` [0]

  it "Should return [0, 1, ..] for sums [1, 2, 10]" do
    (sums [1, 2, 10]) `shouldEqual` [0, 1, 2, 3, 10, 11, 12, 13]
