{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Applicative (empty)
import Control.Monad.Except (catchError, throwError)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.Text (Text)
import SimpleParser
import SimpleParser.Examples.Json (Json (..), JsonF (..), parseJson)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.TH (defaultMainGenerator)

newtype Error = Error { unError :: String } deriving (Eq, Show)

type TestParser a = Parser Error (OffsetStream Text) a

type TestResult a = ParseResult Error (OffsetStream Text) a

data InputOutput a = InputOutput !Text ![TestResult a]

runParserCase :: (Show a, Eq a) => TestParser a -> InputOutput a -> Assertion
runParserCase parser (InputOutput input expected) = do
  let actual = runParser parser (newOffsetStream input)
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
        [ ("empty", InputOutput "" [parseSuccessResult 1 (OffsetStream 0 "")])
        , ("non-empty", InputOutput "hi" [parseSuccessResult 1 (OffsetStream 0 "hi")])
        ]
  in testParserTrees parser cases

test_peek_token :: [TestTree]
test_peek_token =
  let parser = peekToken
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult Nothing (OffsetStream 0 "")])
        , ("match", InputOutput "hi" [parseSuccessResult (Just 'h') (OffsetStream 0 "hi")])
        ]
  in testParserTrees parser cases

test_pop_token :: [TestTree]
test_pop_token =
  let parser = popToken
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult Nothing (OffsetStream 0 "")])
        , ("match", InputOutput "hi" [parseSuccessResult (Just 'h') (OffsetStream 1 "i")])
        ]
  in testParserTrees parser cases

test_peek_chunk :: [TestTree]
test_peek_chunk =
  let parser = peekChunk 2
      cases =
        [ ("len 0", InputOutput "" [parseSuccessResult Nothing (OffsetStream 0 "")])
        , ("len 1", InputOutput "h" [parseSuccessResult (Just "h") (OffsetStream 0 "h")])
        , ("len 2", InputOutput "hi" [parseSuccessResult (Just "hi") (OffsetStream 0 "hi")])
        , ("len 3", InputOutput "hii" [parseSuccessResult (Just "hi") (OffsetStream 0 "hii")])
        ]
  in testParserTrees parser cases

test_pop_chunk :: [TestTree]
test_pop_chunk =
  let parser = popChunk 2
      cases =
        [ ("len 0", InputOutput "" [parseSuccessResult Nothing (OffsetStream 0 "")])
        , ("len 1", InputOutput "h" [parseSuccessResult (Just "h") (OffsetStream 1 "")])
        , ("len 2", InputOutput "hi" [parseSuccessResult (Just "hi") (OffsetStream 2 "")])
        , ("len 3", InputOutput "hii" [parseSuccessResult (Just "hi") (OffsetStream 2 "i")])
        ]
  in testParserTrees parser cases

test_drop_chunk :: [TestTree]
test_drop_chunk =
  let parser = dropChunk 2
      cases =
        [ ("len 0", InputOutput "" [parseSuccessResult Nothing (OffsetStream 0 "")])
        , ("len 1", InputOutput "h" [parseSuccessResult (Just 1) (OffsetStream 1 "")])
        , ("len 2", InputOutput "hi" [parseSuccessResult (Just 2) (OffsetStream 2 "")])
        , ("len 3", InputOutput "hii" [parseSuccessResult (Just 2) (OffsetStream 2 "i")])
        ]
  in testParserTrees parser cases

test_is_end :: [TestTree]
test_is_end =
  let parser = isEnd
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult True (OffsetStream 0 "")])
        , ("non-empty", InputOutput "hi" [parseSuccessResult False (OffsetStream 0 "hi")])
        ]
  in testParserTrees parser cases

test_match_end :: [TestTree]
test_match_end =
  let parser = matchEnd
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult () (OffsetStream 0 "")])
        , ("non-empty", InputOutput "hi" [])
        ]
  in testParserTrees parser cases

test_any_token :: [TestTree]
test_any_token =
  let parser = anyToken
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [parseSuccessResult 'h' (OffsetStream 1 "i")])
        ]
  in testParserTrees parser cases

test_any_chunk :: [TestTree]
test_any_chunk =
  let parser = anyChunk 2 :: TestParser Text
      cases =
        [ ("len 0", InputOutput "" [])
        , ("len 1", InputOutput "h" [parseSuccessResult "h" (OffsetStream 1 "")])
        , ("len 2", InputOutput "hi" [parseSuccessResult "hi" (OffsetStream 2 "")])
        , ("len 3", InputOutput "hii" [parseSuccessResult "hi" (OffsetStream 2 "i")])
        ]
  in testParserTrees parser cases

test_match_token :: [TestTree]
test_match_token =
  let parser = matchToken 'h'
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [parseSuccessResult 'h' (OffsetStream 1 "i")])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_match_chunk :: [TestTree]
test_match_chunk =
  let parser = matchChunk "hi"
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [parseSuccessResult "hi" (OffsetStream 2 "")])
        , ("prefix", InputOutput "hiya" [parseSuccessResult "hi" (OffsetStream 2 "ya")])
        , ("partial", InputOutput "hey" [])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_greedy_star :: [TestTree]
test_greedy_star =
  let parser = greedyStarParser (matchToken 'h')
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult "" (OffsetStream 0 "")])
        , ("non-empty", InputOutput "hi" [parseSuccessResult "h" (OffsetStream 1 "i")])
        , ("repeat", InputOutput "hhi" [parseSuccessResult "hh" (OffsetStream 2 "i")])
        , ("full", InputOutput "hhh" [parseSuccessResult "hhh" (OffsetStream 3 "")])
        , ("non-match", InputOutput "bye" [parseSuccessResult "" (OffsetStream 0 "bye")])
        ]
  in testParserTrees parser cases

test_greedy_star_unit :: [TestTree]
test_greedy_star_unit =
  let parser = greedyStarParser_ (matchToken 'h')
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult () (OffsetStream 0 "")])
        , ("non-empty", InputOutput "hi" [parseSuccessResult () (OffsetStream 1 "i")])
        , ("repeat", InputOutput "hhi" [parseSuccessResult () (OffsetStream 2 "i")])
        , ("full", InputOutput "hhh" [parseSuccessResult () (OffsetStream 3 "")])
        , ("non-match", InputOutput "bye" [parseSuccessResult () (OffsetStream 0 "bye")])
        ]
  in testParserTrees parser cases

test_greedy_plus :: [TestTree]
test_greedy_plus =
  let parser = greedyPlusParser (matchToken 'h')
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [parseSuccessResult "h" (OffsetStream 1 "i")])
        , ("repeat", InputOutput "hhi" [parseSuccessResult "hh" (OffsetStream 2 "i")])
        , ("full", InputOutput "hhh" [parseSuccessResult "hhh" (OffsetStream 3 "")])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_greedy_plus_unit :: [TestTree]
test_greedy_plus_unit =
  let parser = greedyPlusParser_ (matchToken 'h')
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [parseSuccessResult () (OffsetStream 1 "i")])
        , ("repeat", InputOutput "hhi" [parseSuccessResult () (OffsetStream 2 "i")])
        , ("full", InputOutput "hhh" [parseSuccessResult () (OffsetStream 3 "")])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_branch :: [TestTree]
test_branch =
  let parser = branchParser [matchToken 'h', matchToken 'x']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [parseSuccessResult 'h' (OffsetStream 1 "i")])
        , ("second", InputOutput "xi" [parseSuccessResult 'x' (OffsetStream 1 "i")])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_branch_first :: [TestTree]
test_branch_first =
  let parser = branchParser [anyToken $> 'h', matchToken 'x']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [parseSuccessResult 'h' (OffsetStream 1 "i")])
        , ("second", InputOutput "xi" [parseSuccessResult 'h' (OffsetStream 1 "i")])
        ]
  in testParserTrees parser cases

test_branch_second :: [TestTree]
test_branch_second =
  let parser = branchParser [empty, anyToken $> 'x']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [parseSuccessResult 'x' (OffsetStream 1 "i")])
        , ("second", InputOutput "xi" [parseSuccessResult 'x' (OffsetStream 1 "i")])
        ]
  in testParserTrees parser cases

test_combine :: [TestTree]
test_combine =
  let parser = asum [matchToken 'h', matchToken 'x']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [parseSuccessResult 'h' (OffsetStream 1 "i")])
        , ("second", InputOutput "xi" [parseSuccessResult 'x' (OffsetStream 1 "i")])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_combine_first :: [TestTree]
test_combine_first =
  let state = OffsetStream 1 "i"
      parser = asum [anyToken $> 'h', matchToken 'x']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [parseSuccessResult 'h' state])
        , ("second", InputOutput "xi" [parseSuccessResult 'h' state, parseSuccessResult 'x' state])
        ]
  in testParserTrees parser cases

test_combine_second :: [TestTree]
test_combine_second =
  let state = OffsetStream 1 "i"
      parser = asum [empty, anyToken $> 'x']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [parseSuccessResult 'x' state])
        , ("second", InputOutput "xi" [parseSuccessResult 'x' state])
        ]
  in testParserTrees parser cases

test_with_default_empty :: [TestTree]
test_with_default_empty =
  let parser = defaultParser 'z' empty
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult 'z' (OffsetStream 0 "")])
        , ("non-empty", InputOutput "hi" [parseSuccessResult 'z' (OffsetStream 0 "hi")])
        ]
  in testParserTrees parser cases

test_with_default :: [TestTree]
test_with_default =
  let parser = defaultParser 'z' (matchToken 'h')
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult 'z' (OffsetStream 0 "")])
        , ("match", InputOutput "hi" [parseSuccessResult 'h' (OffsetStream 1 "i")])
        , ("non-match", InputOutput "bye" [parseSuccessResult 'z' (OffsetStream 0 "bye")])
        ]
  in testParserTrees parser cases

test_bind_multi_pre :: [TestTree]
test_bind_multi_pre =
  let state = OffsetStream 1 "i"
      parser = asum [anyToken $> 'h', matchToken 'x'] >>= \c -> pure [c, c]
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [parseSuccessResult "hh" state])
        , ("second", InputOutput "xi" [parseSuccessResult "hh" state, parseSuccessResult "xx" state])
        ]
  in testParserTrees parser cases

test_bind_multi_post :: [TestTree]
test_bind_multi_post =
  let state1 = OffsetStream 1 "i"
      state2 = OffsetStream 2 ""
      parser = anyToken >>= \x -> asum [pure x, matchToken 'i']
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
        [ ("empty", InputOutput "" [parseErrorResult err (OffsetStream 0 "")])
        , ("non-empty", InputOutput "hi" [parseErrorResult err (OffsetStream 0 "hi")])
        ]
  in testParserTrees parser cases

test_consume_throw :: [TestTree]
test_consume_throw =
  let err = Error "boo"
      parser = anyToken *> throwError err :: TestParser Int
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [parseErrorResult err (OffsetStream 1 "i")])
        ]
  in testParserTrees parser cases

test_with_default_throw :: [TestTree]
test_with_default_throw =
  let err = Error "boo"
      parser = defaultParser 'z' (throwError err)
      cases =
        [ ("empty", InputOutput "" [parseErrorResult err (OffsetStream 0 "")])
        , ("non-empty", InputOutput "hi" [parseErrorResult err (OffsetStream 0 "hi")])
        ]
  in testParserTrees parser cases

test_with_default_consume_throw :: [TestTree]
test_with_default_consume_throw =
  let err = Error "boo"
      parser = defaultParser 'z' (anyToken *> throwError err)
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult 'z' (OffsetStream 0 "")])
        , ("non-empty", InputOutput "hi" [parseErrorResult err (OffsetStream 1 "i")])
        ]
  in testParserTrees parser cases

test_throw_mixed :: [TestTree]
test_throw_mixed =
  let state = OffsetStream 0 "hi"
      err = Error "boo"
      parser = asum [throwError err, pure 1 :: TestParser Int]
      cases =
        [ ("non-empty", InputOutput "hi" [parseErrorResult err state, parseSuccessResult 1 state])
        ]
  in testParserTrees parser cases

test_catch :: [TestTree]
test_catch =
  let state = OffsetStream 0 "hi"
      err = Error "boo"
      parser = catchError (asum [throwError err, pure 1]) (\(Error m) -> pure (if m == "boo" then 2 else 3)) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 2 state, parseSuccessResult 1 state])
        ]
  in testParserTrees parser cases

test_catch_recur :: [TestTree]
test_catch_recur =
  let state = OffsetStream 0 "hi"
      err1 = Error "boo"
      err2 = Error "two"
      parser = catchError (throwError err1) (const (throwError err2)) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseErrorResult err2 state])
        ]
  in testParserTrees parser cases

test_suppress_success :: [TestTree]
test_suppress_success =
  let state = OffsetStream 0 "hi"
      parser = suppressParser (asum [pure 1, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 1 state, parseSuccessResult 2 state])
        ]
  in testParserTrees parser cases

test_suppress_fail_first :: [TestTree]
test_suppress_fail_first =
  let err = Error "boo"
      parser = suppressParser (asum [throwError err, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 2 (OffsetStream 0 "hi")])
        ]
  in testParserTrees parser cases

test_suppress_fail_second :: [TestTree]
test_suppress_fail_second =
  let err = Error "boo"
      parser = suppressParser (asum [pure 1, throwError err]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 1 (OffsetStream 0 "hi")])
        ]
  in testParserTrees parser cases

test_suppress_fail_both :: [TestTree]
test_suppress_fail_both =
  let state = OffsetStream 0 "hi"
      err1 = Error "boo1"
      err2 = Error "boo2"
      parser = suppressParser (asum [throwError err1, throwError err2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseErrorResult err1 state, parseErrorResult err2 state])
        ]
  in testParserTrees parser cases

test_isolate_success :: [TestTree]
test_isolate_success =
  let state = OffsetStream 0 "hi"
      parser = isolateParser (asum [pure 1, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 1 state])
        ]
  in testParserTrees parser cases

test_isolate_fail_first :: [TestTree]
test_isolate_fail_first =
  let err = Error "boo"
      parser = isolateParser (asum [throwError err, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 2 (OffsetStream 0 "hi")])
        ]
  in testParserTrees parser cases

test_isolate_fail_second :: [TestTree]
test_isolate_fail_second =
  let err = Error "boo"
      parser = isolateParser (asum [pure 1, throwError err]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 1 (OffsetStream 0 "hi")])
        ]
  in testParserTrees parser cases

test_isolate_fail_both :: [TestTree]
test_isolate_fail_both =
  let state = OffsetStream 0 "hi"
      err1 = Error "boo1"
      err2 = Error "boo2"
      parser = isolateParser (asum [throwError err1, throwError err2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseErrorResult err1 state, parseErrorResult err2 state])
        ]
  in testParserTrees parser cases

test_silence_success :: [TestTree]
test_silence_success =
  let state = OffsetStream 0 "hi"
      parser = silenceParser (asum [pure 1, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 1 state, parseSuccessResult 2 state])
        ]
  in testParserTrees parser cases

test_silence_fail_first :: [TestTree]
test_silence_fail_first =
  let err = Error "boo"
      parser = silenceParser (asum [throwError err, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 2 (OffsetStream 0 "hi")])
        ]
  in testParserTrees parser cases

test_silence_fail_second :: [TestTree]
test_silence_fail_second =
  let err = Error "boo"
      parser = silenceParser (asum [pure 1, throwError err]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [parseSuccessResult 1 (OffsetStream 0 "hi")])
        ]
  in testParserTrees parser cases

test_silence_fail_both :: [TestTree]
test_silence_fail_both =
  let err1 = Error "boo1"
      err2 = Error "boo2"
      parser = silenceParser (asum [throwError err1, throwError err2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [])
        ]
  in testParserTrees parser cases

test_look_ahead_success :: [TestTree]
test_look_ahead_success =
  let parser = lookAheadParser anyToken
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [parseSuccessResult 'h' (OffsetStream 0 "hi")])
        ]
  in testParserTrees parser cases

test_look_ahead_failure :: [TestTree]
test_look_ahead_failure =
  let err = Error "boo"
      parser = lookAheadParser (anyToken *> throwError err) :: TestParser Char
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [parseErrorResult err (OffsetStream 0 "hi")])
        ]
  in testParserTrees parser cases

test_take_while :: [TestTree]
test_take_while =
  let parser = takeTokensWhile (=='h') :: TestParser Text
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult "" (OffsetStream 0 "")])
        , ("non-match", InputOutput "i" [parseSuccessResult "" (OffsetStream 0 "i")])
        , ("match", InputOutput "hi" [parseSuccessResult "h" (OffsetStream 1 "i")])
        , ("match 2", InputOutput "hhi" [parseSuccessResult "hh" (OffsetStream 2 "i")])
        , ("match end", InputOutput "hh" [parseSuccessResult "hh" (OffsetStream 2 "")])
        ]
  in testParserTrees parser cases

test_take_while_1 :: [TestTree]
test_take_while_1 =
  let parser = takeTokensWhile1 (=='h') :: TestParser Text
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-match", InputOutput "i" [])
        , ("match", InputOutput "hi" [parseSuccessResult "h" (OffsetStream 1 "i")])
        , ("match 2", InputOutput "hhi" [parseSuccessResult "hh" (OffsetStream 2 "i")])
        , ("match end", InputOutput "hh" [parseSuccessResult "hh" (OffsetStream 2 "")])
        ]
  in testParserTrees parser cases

test_drop_while :: [TestTree]
test_drop_while =
  let parser = dropTokensWhile (=='h') :: TestParser Int
      cases =
        [ ("empty", InputOutput "" [parseSuccessResult 0 (OffsetStream 0 "")])
        , ("non-match", InputOutput "i" [parseSuccessResult 0 (OffsetStream 0 "i")])
        , ("match", InputOutput "hi" [parseSuccessResult 1 (OffsetStream 1 "i")])
        , ("match 2", InputOutput "hhi" [parseSuccessResult 2 (OffsetStream 2 "i")])
        , ("match end", InputOutput "hh" [parseSuccessResult 2 (OffsetStream 2 "")])
        ]
  in testParserTrees parser cases

test_drop_while_1 :: [TestTree]
test_drop_while_1 =
  let parser = dropTokensWhile1 (=='h') :: TestParser Int
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-match", InputOutput "i" [])
        , ("match", InputOutput "hi" [parseSuccessResult 1 (OffsetStream 1 "i")])
        , ("match 2", InputOutput "hhi" [parseSuccessResult 2 (OffsetStream 2 "i")])
        , ("match end", InputOutput "hh" [parseSuccessResult 2 (OffsetStream 2 "")])
        ]
  in testParserTrees parser cases

testJsonCase :: TestName -> Text -> [Json] -> TestTree
testJsonCase name str expected = testCase ("json " <> name) $ do
  let actual = parseJson str
  actual @?= expected

testJsonTrees :: [(TestName, Text, [Json])] -> [TestTree]
testJsonTrees = fmap (\(n, s, e) -> testJsonCase n s e)

test_json :: [TestTree]
test_json =
  let nullVal = Json JsonNull
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
        -- TODO(ejconlon) Refine parser to make this pass
        -- , ("str4", "\"xy\\\"z\"", [strVal "xy\"z"])
        , ("obj0", "{}", [objVal []])
        , ("obj1", "{\"x\": true}", [objVal [("x", trueVal)]])
        , ("obj2", "{\"x\": true, \"y\": false}", [objVal [("x", trueVal), ("y", falseVal)]])
        ]
  in testJsonTrees cases

main :: IO ()
main = $(defaultMainGenerator)
