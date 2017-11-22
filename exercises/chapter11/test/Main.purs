module Test.Main where

import Exercises
import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (execState, runState)
import Control.Monad.Writer (execWriter, runWriter)
import Data.Array (many, some)
import Data.Either (Either(..))
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

  it "should safely divide 10 by 4" do
    unwrap (runExceptT (safeDivide 10.0 4.0)) `shouldEqual` (Right 2.5)

  it "should safely divide 10 by 0" do
    unwrap (runExceptT (safeDivide 10.0 0.0)) `shouldEqual` (Left "#DIV/0!")

  it "should run split example 1" do
    runParser split "test" `shouldEqual` (Right (Tuple (Tuple "t" "est") ["The state is test"]))

  it "should run split example 2" do
    runParser ((<>) <$> split <*> split) "test" `shouldEqual`
      (Right (Tuple (Tuple "te" "st") ["The state is test", "The state is est"]))

  it "should run split example 3" do
    runParser split "" `shouldEqual` (Left ["Empty string"])

  it "should match a prefix" do
     runParser (string "abc") "abcdef" `shouldEqual`
       (Right (Tuple (Tuple "abc" "def") ["The state is abcdef"]))

  it "should not match an empty string" do
     runParser (string "abc") "" `shouldEqual` (Left ["Empty string"])

  it "should not match a wrong prefix" do
     runParser (string "xyz") "abcdef" `shouldEqual`
       (Left ["String does not start with prefix"])

  it "should many split" do
    runParser (many split) "test" `shouldEqual`
      (Right (Tuple (Tuple ["t", "e", "s", "t"] "")
        [ "The state is test"
        , "The state is est"
        , "The state is st"
        , "The state is t"
        ]))

  it "should many string" do
    runParser (many (string "a")) "aabbcc" `shouldEqual`
      (Right (Tuple (Tuple ["a", "a"] "bbcc")
        [ "The state is aabbcc"
        , "The state is abbcc"
        ]))

  it "should many a many b string" do
    let parser = (many (some (string "a") <|> some (string "b")))
    runParser parser "aabbcc" `shouldEqual`
      (Right (Tuple (Tuple [["a", "a"], ["b", "b"]] "cc")
        [ "The state is aabbcc"
        , "The state is abbcc"
        , "The state is bbcc"
        , "The state is bcc"
        ]))

  it "should char x" do
    runParser (char 'x') "xyz" `shouldEqual`
      (Right (Tuple (Tuple "x" "yz")
        [ "The state is xyz"
        ]))

  it "should not char y" do
    runParser (char 'y') "xyz" `shouldEqual`
      (Left ["String does not start with char"])

  it "should not char x with empty string" do
    runParser (char 'x') "" `shouldEqual`
      (Left ["String does not start with char"])

  it "should parse a's and b's" do
    let parser = many (char 'a' <|> char 'b')
    runParser parser "abbacddc" `shouldEqual`
      (Right (Tuple (Tuple ["a", "b", "b", "a"] "cddc")
        [ "The state is abbacddc"
        , "The state is bbacddc"
        , "The state is bacddc"
        , "The state is acddc"
        ]))
