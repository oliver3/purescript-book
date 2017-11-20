module Test.Main where

import Exercises
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.State (execState, runState)
import Control.Monad.Writer (execWriter, runWriter)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  it "should sum all" do
    (Tuple "21" 21) `shouldEqual` runState (do
      _ <- sumArray [1, 2, 3]
      _ <- sumArray [4, 5]
      sumArray [6]) 0

  it "should allow empty string" do
    (testParens "") `shouldEqual` true

  it "should work" do
    (testParens "(()(())())") `shouldEqual` true

  it "should work'" do
    (execState (balanseParens "(()(())())") 0 ) `shouldEqual` 0

  it "should work 1" do
    (execState (balanseParens "(") 0 ) `shouldEqual` 1

  it "should work 2" do
    (execState (balanseParens "()") 0 ) `shouldEqual` 0

  it "should work 2'" do
    (execState (balanseParens "((") 0 ) `shouldEqual` 2

  it "should work 3'" do
    (execState (balanseParens "(()") 0 ) `shouldEqual` 1

  it "should work 4'" do
    (execState (balanseParens "(())") 0 ) `shouldEqual` 0


  it "should detect too many closing" do
    (testParens ")") `shouldEqual` false

  it "should detect too many opening" do
    (testParens "(()()") `shouldEqual` false

  it "should render the doc" do
    let
      expected = joinWith "\n"
        [ "Here is some indented text:"
        , "  I am indented"
        , "  So am I"
        , "    I am even more indented"
        ]
      actual = render $ cat
        [ line "Here is some indented text:"
        , indent $ cat
          [ line "I am indented"
          , line "So am I"
          , indent $ line "I am even more indented"
          ]
        ]
    actual `shouldEqual` expected

  it "should add with Writer" do
    execWriter (sumArray' [1, 2, 3, 4]) `shouldEqual` Additive 10

  it "should calculate collatz 1" do
    collatz 1 `shouldEqual` 0

  it "should calculate collatz 10" do
    collatz 10 `shouldEqual` 6

  it "should calculate with Writer collatz 1" do
    runWriter (collatz' 1) `shouldEqual` (Tuple 0 [1])

  it "should calculate with Writer collatz 10" do
    runWriter (collatz' 10) `shouldEqual` (Tuple 6 [10, 5, 16, 8, 4, 2, 1])

