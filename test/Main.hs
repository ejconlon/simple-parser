{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Applicative (Alternative (..))
import Control.Monad.Except (MonadError (..))
import Data.Foldable (asum)
import Prelude
import SimpleParser
import SimpleParser.Examples.Json (Json (..), JsonF (..), parseJson)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.TH (defaultMainGenerator)

newtype Error = Error { unError :: String } deriving (Eq, Show)

-- TODO Rename to TestInput once all test cases are migrated
type TestParser a = Input Char Error StringStreamState a

type TestResult a = ParseResult Error StringStreamState a

data InputOutput a = InputOutput !String ![TestResult a]

runParserCase :: (Show a, Eq a) => TestParser a -> InputOutput a -> Assertion
runParserCase parser (InputOutput input expected) = do
  let actual = runInput parser stringStream (newStringStreamState input)
  actual @?= expected

testParserCase :: (Show a, Eq a) => TestName -> TestParser a -> InputOutput a -> TestTree
testParserCase name parser inOut = testCase name (runParserCase parser inOut)

testParserTrees :: (Show a, Eq a) => TestParser a -> [(TestName, InputOutput a)] -> [TestTree]
testParserTrees parser = fmap (\(n, io) -> testParserCase n parser io)

test_empty :: [TestTree]
test_empty =
  let parser = empty :: TestParser Int
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [])
        ]
  in testParserTrees parser cases

test_pure :: [TestTree]
test_pure =
  let parser = pure (1 :: Int)
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult 1 (ListStreamState 0 "")])
        , ("non-empty", InputOutput "hi" [parseSuccessResult 1 (ListStreamState 0 "hi")])
        ]
  in testParserTrees parser cases

test_peek :: [TestTree]
test_peek =
  let parser = peekInput
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult Nothing (ListStreamState 0 "")])
        , ("match", InputOutput "hi" [parseSuccessResult (Just 'h') (ListStreamState 0 "hi")])
        ]
  in testParserTrees parser cases

test_pop :: [TestTree]
test_pop =
  let parser = popInput
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult Nothing (ListStreamState 0 "")])
        , ("match", InputOutput "hi" [parseSuccessResult (Just 'h') (ListStreamState 1 "i")])
        ]
  in testParserTrees parser cases

test_end :: [TestTree]
test_end =
  let parser = endInput
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult () (ListStreamState 0 "")])
        , ("non-empty", InputOutput "hi" [])
        ]
  in testParserTrees parser cases

test_skip :: [TestTree]
test_skip =
  let parser = skipInput
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [parseSuccessResult 'h' (ListStreamState 1 "i")])
        ]
  in testParserTrees parser cases

test_char :: [TestTree]
test_char =
  let parser = charInput 'h'
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [parseSuccessResult 'h' (ListStreamState 1 "i")])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_word :: [TestTree]
test_word =
  let parser = wordInput "hi"
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [parseSuccessResult "hi" (ListStreamState 2 "")])
        , ("prefix", InputOutput "hiya" [parseSuccessResult "hi" (ListStreamState 2 "ya")])
        , ("partial", InputOutput "hey" [])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_greedy_star :: [TestTree]
test_greedy_star =
  let parser = greedyStarInput (charInput 'h')
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult "" (ListStreamState 0 "")])
        , ("non-empty", InputOutput "hi" [parseSuccessResult "h" (ListStreamState 1 "i")])
        , ("repeat", InputOutput "hhi" [parseSuccessResult "hh" (ListStreamState 2 "i")])
        , ("full", InputOutput "hhh" [parseSuccessResult "hhh" (ListStreamState 3 "")])
        , ("non-match", InputOutput "bye" [parseSuccessResult "" (ListStreamState 0 "bye")])
        ]
  in testParserTrees parser cases

test_greedy_star_unit :: [TestTree]
test_greedy_star_unit =
  let parser = greedyStarInput_ (charInput 'h')
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult () (ListStreamState 0 "")])
        , ("non-empty", InputOutput "hi" [parseSuccessResult () (ListStreamState 1 "i")])
        , ("repeat", InputOutput "hhi" [parseSuccessResult () (ListStreamState 2 "i")])
        , ("full", InputOutput "hhh" [parseSuccessResult () (ListStreamState 3 "")])
        , ("non-match", InputOutput "bye" [parseSuccessResult () (ListStreamState 0 "bye")])
        ]
  in testParserTrees parser cases

test_greedy_plus :: [TestTree]
test_greedy_plus =
  let parser = greedyPlusInput (charInput 'h')
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [parseSuccessResult "h" (ListStreamState 1 "i")])
        , ("repeat", InputOutput "hhi" [parseSuccessResult "hh" (ListStreamState 2 "i")])
        , ("full", InputOutput "hhh" [parseSuccessResult "hhh" (ListStreamState 3 "")])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_greedy_plus_unit :: [TestTree]
test_greedy_plus_unit =
  let parser = greedyPlusInput_ (charInput 'h')
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [parseSuccessResult () (ListStreamState 1 "i")])
        , ("repeat", InputOutput "hhi" [parseSuccessResult () (ListStreamState 2 "i")])
        , ("full", InputOutput "hhh" [parseSuccessResult () (ListStreamState 3 "")])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_branch :: [TestTree]
test_branch =
  let parser = branchInput [charInput 'h', charInput 'x']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [parseSuccessResult 'h' (ListStreamState 1 "i")])
        , ("second", InputOutput "xi" [parseSuccessResult 'x' (ListStreamState 1 "i")])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_branch_first :: [TestTree]
test_branch_first =
  let parser = branchInput [skipInput *> pure 'h', charInput 'x']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [parseSuccessResult 'h' (ListStreamState 1 "i")])
        , ("second", InputOutput "xi" [parseSuccessResult 'h' (ListStreamState 1 "i")])
        ]
  in testParserTrees parser cases

test_branch_second :: [TestTree]
test_branch_second =
  let parser = branchInput [empty, skipInput *> pure 'x']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [parseSuccessResult 'x' (ListStreamState 1 "i")])
        , ("second", InputOutput "xi" [parseSuccessResult 'x' (ListStreamState 1 "i")])
        ]
  in testParserTrees parser cases

test_combine :: [TestTree]
test_combine =
  let parser = asum [charInput 'h', charInput 'x']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [parseSuccessResult 'h' (ListStreamState 1 "i")])
        , ("second", InputOutput "xi" [parseSuccessResult 'x' (ListStreamState 1 "i")])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_combine_first :: [TestTree]
test_combine_first =
  let state = ListStreamState 1 "i"
      parser = asum [skipInput *> pure 'h', charInput 'x']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [parseSuccessResult 'h' state])
        , ("second", InputOutput "xi" [parseSuccessResult 'h' state, parseSuccessResult 'x' state])
        ]
  in testParserTrees parser cases

test_combine_second :: [TestTree]
test_combine_second =
  let state = ListStreamState 1 "i"
      parser = asum [empty, skipInput *> pure 'x']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [parseSuccessResult 'x' state])
        , ("second", InputOutput "xi" [parseSuccessResult 'x' state])
        ]
  in testParserTrees parser cases

test_with_default_empty :: [TestTree]
test_with_default_empty =
  let parser = defaultInput 'z' empty
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult 'z' (ListStreamState 0 "")])
        , ("non-empty", InputOutput "hi" [parseSuccessResult 'z' (ListStreamState 0 "hi")])
        ]
  in testParserTrees parser cases

test_with_default :: [TestTree]
test_with_default =
  let parser = defaultInput 'z' (charInput 'h')
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult 'z' (ListStreamState 0 "")])
        , ("match", InputOutput "hi" [parseSuccessResult 'h' (ListStreamState 1 "i")])
        , ("non-match", InputOutput "bye" [parseSuccessResult 'z' (ListStreamState 0 "bye")])
        ]
  in testParserTrees parser cases

test_bind_multi_pre :: [TestTree]
test_bind_multi_pre =
  let state = ListStreamState 1 "i"
      parser = asum [skipInput *> pure 'h', charInput 'x'] >>= \c -> pure [c, c]
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [parseSuccessResult "hh" state])
        , ("second", InputOutput "xi" [parseSuccessResult "hh" state, parseSuccessResult "xx" state])
        ]
  in testParserTrees parser cases

test_bind_multi_post :: [TestTree]
test_bind_multi_post =
  let state1 = ListStreamState 1 "i"
      state2 = ListStreamState 2 ""
      parser = skipInput >>= \x -> asum [pure x, charInput 'i']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [parseSuccessResult 'h' state1, parseSuccessResult 'i' state2])
        , ("second", InputOutput "xi" [parseSuccessResult 'x' state1, parseSuccessResult 'i' state2])
        ]
  in testParserTrees parser cases

test_throw :: [TestTree]
test_throw =
  let err = Error "boo"
      parser = throwError err :: TestParser Int
      cases =
        [ ("empty", InputOutput "" [parseErrorResult err (ListStreamState 0 "")])
        , ("non-empty", InputOutput "hi" [parseErrorResult err (ListStreamState 0 "hi")])
        ]
  in testParserTrees parser cases

test_consume_throw :: [TestTree]
test_consume_throw =
  let err = Error "boo"
      parser = skipInput *> throwError err :: TestParser Int
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [parseErrorResult err (ListStreamState 1 "i")])
        ]
  in testParserTrees parser cases

test_with_default_throw :: [TestTree]
test_with_default_throw =
  let err = Error "boo"
      parser = defaultInput 'z' (throwError err)
      cases =
        [ ("empty", InputOutput "" [parseErrorResult err (ListStreamState 0 "")])
        , ("non-empty", InputOutput "hi" [parseErrorResult err (ListStreamState 0 "hi")])
        ]
  in testParserTrees parser cases

test_with_default_consume_throw :: [TestTree]
test_with_default_consume_throw =
  let err = Error "boo"
      parser = defaultInput 'z' (skipInput *> throwError err)
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult 'z' (ListStreamState 0 "")])
        , ("non-empty", InputOutput "hi" [parseErrorResult err (ListStreamState 1 "i")])
        ]
  in testParserTrees parser cases

test_throw_mixed :: [TestTree]
test_throw_mixed =
  let state = ListStreamState 0 "hi"
      err = Error "boo"
      parser = asum [throwError err, pure 1 :: TestParser Int]
      cases =
        [ ("non-empty", InputOutput "hi" [parseErrorResult err state, parseSuccessResult 1 state])
        ]
  in testParserTrees parser cases

test_catch :: [TestTree]
test_catch =
  let state = ListStreamState 0 "hi"
      err = Error "boo"
      parser = catchError (asum [throwError err, pure 1]) (\(Error m) -> pure (if m == "boo" then 2 else 3)) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 2 state, parseSuccessResult 1 state])
        ]
  in testParserTrees parser cases

test_catch_recur :: [TestTree]
test_catch_recur =
  let state = ListStreamState 0 "hi"
      err1 = Error "boo"
      err2 = Error "two"
      parser = catchError (throwError err1) (const (throwError err2)) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseErrorResult err2 state])
        ]
  in testParserTrees parser cases

test_suppress_success :: [TestTree]
test_suppress_success =
  let state = ListStreamState 0 "hi"
      parser = suppressInput (asum [pure 1, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 1 state, parseSuccessResult 2 state])
        ]
  in testParserTrees parser cases

test_suppress_fail_first :: [TestTree]
test_suppress_fail_first =
  let err = Error "boo"
      parser = suppressInput (asum [throwError err, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 2 (ListStreamState 0 "hi")])
        ]
  in testParserTrees parser cases

test_suppress_fail_second :: [TestTree]
test_suppress_fail_second =
  let err = Error "boo"
      parser = suppressInput (asum [pure 1, throwError err]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 1 (ListStreamState 0 "hi")])
        ]
  in testParserTrees parser cases

test_suppress_fail_both :: [TestTree]
test_suppress_fail_both =
  let state = ListStreamState 0 "hi"
      err1 = Error "boo1"
      err2 = Error "boo2"
      parser = suppressInput (asum [throwError err1, throwError err2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseErrorResult err1 state, parseErrorResult err2 state])
        ]
  in testParserTrees parser cases

test_silence_success :: [TestTree]
test_silence_success =
  let state = ListStreamState 0 "hi"
      parser = silenceInput (asum [pure 1, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 1 state, parseSuccessResult 2 state])
        ]
  in testParserTrees parser cases

test_silence_fail_first :: [TestTree]
test_silence_fail_first =
  let err = Error "boo"
      parser = silenceInput (asum [throwError err, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 2 (ListStreamState 0 "hi")])
        ]
  in testParserTrees parser cases

test_silence_fail_second :: [TestTree]
test_silence_fail_second =
  let err = Error "boo"
      parser = silenceInput (asum [pure 1, throwError err]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 1 (ListStreamState 0 "hi")])
        ]
  in testParserTrees parser cases

test_silence_fail_both :: [TestTree]
test_silence_fail_both =
  let err1 = Error "boo1"
      err2 = Error "boo2"
      parser = silenceInput (asum [throwError err1, throwError err2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [])
        ]
  in testParserTrees parser cases

test_take_while :: [TestTree]
test_take_while =
  let parser = takeInputWhile (=='h') :: TestParser String
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult "" (ListStreamState 0 "")])
        , ("non-match", InputOutput "i" [parseSuccessResult "" (ListStreamState 0 "i")])
        , ("match", InputOutput "hi" [parseSuccessResult "h" (ListStreamState 1 "i")])
        , ("match 2", InputOutput "hhi" [parseSuccessResult "hh" (ListStreamState 2 "i")])
        , ("match end", InputOutput "hh" [parseSuccessResult "hh" (ListStreamState 2 "")])
        ]
  in testParserTrees parser cases

test_drop_while :: [TestTree]
test_drop_while =
  let parser = dropInputWhile (=='h') :: TestParser ()
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult () (ListStreamState 0 "")])
        , ("non-match", InputOutput "i" [parseSuccessResult () (ListStreamState 0 "i")])
        , ("match", InputOutput "hi" [parseSuccessResult () (ListStreamState 1 "i")])
        , ("match 2", InputOutput "hhi" [parseSuccessResult () (ListStreamState 2 "i")])
        , ("match end", InputOutput "hh" [parseSuccessResult () (ListStreamState 2 "")])
        ]
  in testParserTrees parser cases

testJsonCase :: TestName -> String -> [Json] -> TestTree
testJsonCase name str expected = testCase ("json " <> name) $ do
  let actual = parseJson str
  actual @?= expected

testJsonTrees :: [(TestName, String, [Json])] -> [TestTree]
testJsonTrees = fmap (\(n, s, e) -> testJsonCase n s e)

test_json :: [TestTree]
test_json =
  let nullVal = Json (JsonNull)
      trueVal = Json (JsonBool True)
      falseVal = Json (JsonBool False)
      arrVal = Json . JsonArray
      strVal = Json . JsonString
      objVal = Json . JsonObject
      cases =
        [ ("empty", "", [])
        , ("bad", "bad", [])
        , ("null", "null", [nullVal])
        , ("true", "true", [trueVal])
        , ("false", "false", [falseVal])
        , ("arr0", "[]", [arrVal []])
        , ("arr1", "[null]", [arrVal [nullVal]])
        , ("arr2", "[null, false]", [arrVal [nullVal, falseVal]])
        , ("arr3", "[null, false, true]", [arrVal [nullVal, falseVal, trueVal]])
        , ("arrx", "[null,]", [])
        , ("str0", "\"\"", [strVal ""])
        , ("str1", "\"x\"", [strVal "x"])
        , ("str2", "\"xy\"", [strVal "xy"])
        , ("str3", "\"xyz\"", [strVal "xyz"])
        , ("obj0", "{}", [objVal []])
        , ("obj1", "{\"x\": true}", [objVal [("x", trueVal)]])
        , ("obj2", "{\"x\": true, \"y\": false}", [objVal [("x", trueVal), ("y", falseVal)]])
        ]
  in testJsonTrees cases

main :: IO ()
main = $(defaultMainGenerator)
