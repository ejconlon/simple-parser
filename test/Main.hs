{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Applicative (empty)
import Control.Monad.Except (catchError, throwError)
import Data.Bifunctor (first)
import Data.Foldable (asum)
import qualified Data.Sequence as Seq
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import SimpleParser
import SimpleParser.Examples.Json (Json (..), JsonF (..), JsonLabel, jsonParser)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.TH (defaultMainGenerator)

newtype Label = Label { unLabel :: String } deriving (Eq, Show, IsString)

newtype Error = Error { unError :: String } deriving (Eq, Show, IsString)

type TestState = OffsetStream Text

type TestParser a = Parser Label TestState Error a

type TestResult a = ParseResult Label TestState Error a

type TestRawError = RawError (CompoundLabel Label) Text Char

type TestParseError = ParseError Label TestState Error

data InputOutput a = InputOutput !Text ![TestResult a]

mkUnlabelledCustomError :: Error -> ParseError Label TestState Error
mkUnlabelledCustomError = mkUnlabelledParseError

mkUnlabelledCustomResult :: TestState -> Error -> TestResult a
mkUnlabelledCustomResult s = mkParseErrorResult s . mkUnlabelledCustomError

mkUnlabelledStreamError :: TestRawError -> ParseError Label TestState Error
mkUnlabelledStreamError = mkUnlabelledError . CompoundErrorStream . StreamError

mkUnlabelledStreamResult :: TestState -> TestRawError -> TestResult a
mkUnlabelledStreamResult s = mkParseErrorResult s . mkUnlabelledStreamError

runParserCase :: (Show a, Eq a) => TestParser a -> InputOutput a -> Assertion
runParserCase parser (InputOutput input expected) = do
  let actual = runParser parser emptyLabelStack (newOffsetStream input)
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
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") 1])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") 1])
        ]
  in testParserTrees parser cases

test_peek_token :: [TestTree]
test_peek_token =
  let parser = peekToken
      cases =
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") Nothing])
        , ("match", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") (Just 'h')])
        ]
  in testParserTrees parser cases

test_pop_token :: [TestTree]
test_pop_token =
  let parser = popToken
      cases =
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") Nothing])
        , ("match", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") (Just 'h')])
        ]
  in testParserTrees parser cases

test_peek_chunk :: [TestTree]
test_peek_chunk =
  let parser = peekChunk 2
      cases =
        [ ("len 0", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") Nothing])
        , ("len 1", InputOutput "h" [mkParseSuccessResult (OffsetStream 0 "h") (Just "h")])
        , ("len 2", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") (Just "hi")])
        , ("len 3", InputOutput "hii" [mkParseSuccessResult (OffsetStream 0 "hii") (Just "hi")])
        ]
  in testParserTrees parser cases

test_pop_chunk :: [TestTree]
test_pop_chunk =
  let parser = popChunk 2
      cases =
        [ ("len 0", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") Nothing])
        , ("len 1", InputOutput "h" [mkParseSuccessResult (OffsetStream 1 "") (Just "h")])
        , ("len 2", InputOutput "hi" [mkParseSuccessResult (OffsetStream 2 "") (Just "hi")])
        , ("len 3", InputOutput "hii" [mkParseSuccessResult (OffsetStream 2 "i") (Just "hi")])
        ]
  in testParserTrees parser cases

test_drop_chunk :: [TestTree]
test_drop_chunk =
  let parser = dropChunk 2
      cases =
        [ ("len 0", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") Nothing])
        , ("len 1", InputOutput "h" [mkParseSuccessResult (OffsetStream 1 "") (Just 1)])
        , ("len 2", InputOutput "hi" [mkParseSuccessResult (OffsetStream 2 "") (Just 2)])
        , ("len 3", InputOutput "hii" [mkParseSuccessResult (OffsetStream 2 "i") (Just 2)])
        ]
  in testParserTrees parser cases

test_is_end :: [TestTree]
test_is_end =
  let parser = isEnd
      cases =
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") True])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") False])
        ]
  in testParserTrees parser cases

test_match_end :: [TestTree]
test_match_end =
  let parser = matchEnd
      cases =
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") ()])
        , ("non-empty", InputOutput "hi" [mkUnlabelledStreamResult (OffsetStream 0 "hi") (RawErrorMatchEnd 'h')])
        ]
  in testParserTrees parser cases

test_any_token :: [TestTree]
test_any_token =
  let parser = anyToken
      cases =
        [ ("empty", InputOutput "" [mkUnlabelledStreamResult (OffsetStream 0 "") RawErrorAnyToken])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") 'h'])
        ]
  in testParserTrees parser cases

test_any_chunk :: [TestTree]
test_any_chunk =
  let parser = anyChunk 2 :: TestParser Text
      cases =
        [ ("len 0", InputOutput "" [mkUnlabelledStreamResult (OffsetStream 0 "") RawErrorAnyChunk])
        , ("len 1", InputOutput "h" [mkParseSuccessResult (OffsetStream 1 "") "h"])
        , ("len 2", InputOutput "hi" [mkParseSuccessResult (OffsetStream 2 "") "hi"])
        , ("len 3", InputOutput "hii" [mkParseSuccessResult (OffsetStream 2 "i") "hi"])
        ]
  in testParserTrees parser cases

test_match_token :: [TestTree]
test_match_token =
  let parser = matchToken 'h'
      cases =
        [ ("empty", InputOutput "" [mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchToken 'h' Nothing)])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") 'h'])
        , ("non-match", InputOutput "bye" [mkUnlabelledStreamResult (OffsetStream 0 "bye") (RawErrorMatchToken 'h' (Just 'b'))])
        ]
  in testParserTrees parser cases

test_match_chunk :: [TestTree]
test_match_chunk =
  let parser = matchChunk "hi"
      cases =
        [ ("empty", InputOutput "" [mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchChunk "hi" Nothing)])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 2 "") "hi"])
        , ("prefix", InputOutput "hiya" [mkParseSuccessResult (OffsetStream 2 "ya") "hi"])
        , ("partial", InputOutput "hey" [mkUnlabelledStreamResult (OffsetStream 0 "hey") (RawErrorMatchChunk "hi" (Just "he"))])
        , ("non-match", InputOutput "bye" [mkUnlabelledStreamResult (OffsetStream 0 "bye") (RawErrorMatchChunk "hi" (Just "by"))])
        ]
  in testParserTrees parser cases

test_greedy_star :: [TestTree]
test_greedy_star =
  let parser = greedyStarParser (matchToken 'h') :: TestParser String
      cases =
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") ""])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") "h"])
        , ("repeat", InputOutput "hhi" [mkParseSuccessResult (OffsetStream 2 "i") "hh"])
        , ("full", InputOutput "hhh" [mkParseSuccessResult (OffsetStream 3 "") "hhh"])
        , ("non-match", InputOutput "bye" [mkParseSuccessResult (OffsetStream 0 "bye") ""])
        ]
  in testParserTrees parser cases

test_greedy_star_unit :: [TestTree]
test_greedy_star_unit =
  let parser = greedyStarParser_ (matchToken 'h')
      cases =
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") ()])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") ()])
        , ("repeat", InputOutput "hhi" [mkParseSuccessResult (OffsetStream 2 "i") ()])
        , ("full", InputOutput "hhh" [mkParseSuccessResult (OffsetStream 3 "") ()])
        , ("non-match", InputOutput "bye" [mkParseSuccessResult (OffsetStream 0 "bye") ()])
        ]
  in testParserTrees parser cases

test_greedy_plus :: [TestTree]
test_greedy_plus =
  let parser = greedyPlusParser (matchToken 'h') :: TestParser String
      cases =
        [ ("empty", InputOutput "" [mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchToken 'h' Nothing)])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") "h"])
        , ("repeat", InputOutput "hhi" [mkParseSuccessResult (OffsetStream 2 "i") "hh"])
        , ("full", InputOutput "hhh" [mkParseSuccessResult (OffsetStream 3 "") "hhh"])
        , ("non-match", InputOutput "bye" [mkUnlabelledStreamResult (OffsetStream 0 "bye") (RawErrorMatchToken 'h' (Just 'b'))])
        ]
  in testParserTrees parser cases

test_greedy_plus_unit :: [TestTree]
test_greedy_plus_unit =
  let parser = greedyPlusParser_ (matchToken 'h')
      cases =
        [ ("empty", InputOutput "" [mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchToken 'h' Nothing)])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") ()])
        , ("repeat", InputOutput "hhi" [mkParseSuccessResult (OffsetStream 2 "i") ()])
        , ("full", InputOutput "hhh" [mkParseSuccessResult (OffsetStream 3 "") ()])
        , ("non-match", InputOutput "bye" [mkUnlabelledStreamResult (OffsetStream 0 "bye") (RawErrorMatchToken 'h' (Just 'b'))])
        ]
  in testParserTrees parser cases

test_or :: [TestTree]
test_or =
  let parser = orParser (matchToken 'h') (matchToken 'x')
      cases =
        [ ("empty", InputOutput "" [
            mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchToken 'h' Nothing),
            mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchToken 'x' Nothing)])
        , ("first", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") 'h'])
        , ("second", InputOutput "xi" [mkParseSuccessResult (OffsetStream 1 "i") 'x'])
        , ("non-match", InputOutput "bye" [
            mkUnlabelledStreamResult (OffsetStream 0 "bye") (RawErrorMatchToken 'h' (Just 'b')),
            mkUnlabelledStreamResult (OffsetStream 0 "bye") (RawErrorMatchToken 'x' (Just 'b'))])
        ]
  in testParserTrees parser cases

test_or_all :: [TestTree]
test_or_all =
  let state = OffsetStream 1 "i"
      parser = orAllParser [matchToken 'h', 'y' <$ anyToken, matchToken 'x']
      cases =
        [ ("empty", InputOutput "" [
            mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchToken 'h' Nothing),
            mkUnlabelledStreamResult (OffsetStream 0 "") RawErrorAnyToken,
            mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchToken 'x' Nothing)])
        , ("first", InputOutput "hi" [mkParseSuccessResult state 'h'])
        , ("middle", InputOutput "zi" [mkParseSuccessResult state 'y'])
        , ("last", InputOutput "xi" [mkParseSuccessResult state 'y'])
        ]
  in testParserTrees parser cases

test_and :: [TestTree]
test_and =
  let parser = andParser (matchToken 'h') (matchToken 'x')
      cases =
        [ ("empty", InputOutput "" [
            mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchToken 'h' Nothing),
            mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchToken 'x' Nothing)])
        , ("first", InputOutput "hi" [
            mkParseSuccessResult (OffsetStream 1 "i") 'h',
            mkUnlabelledStreamResult (OffsetStream 0 "hi") (RawErrorMatchToken 'x' (Just 'h'))])
        , ("second", InputOutput "xi" [
            mkUnlabelledStreamResult (OffsetStream 0 "xi") (RawErrorMatchToken 'h' (Just 'x')),
            mkParseSuccessResult (OffsetStream 1 "i") 'x'])
        , ("non-match", InputOutput "bye" [
            mkUnlabelledStreamResult (OffsetStream 0 "bye") (RawErrorMatchToken 'h' (Just 'b')),
            mkUnlabelledStreamResult (OffsetStream 0 "bye") (RawErrorMatchToken 'x' (Just 'b'))])
        ]
  in testParserTrees parser cases

test_and_first :: [TestTree]
test_and_first =
  let state = OffsetStream 1 "i"
      parser = andParser ('h' <$ anyToken) (matchToken 'x')
      cases =
        [ ("empty", InputOutput "" [
            mkUnlabelledStreamResult (OffsetStream 0 "") RawErrorAnyToken,
            mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchToken 'x' Nothing)])
        , ("first", InputOutput "hi" [
            mkParseSuccessResult state 'h',
            mkUnlabelledStreamResult (OffsetStream 0 "hi") (RawErrorMatchToken 'x' (Just 'h'))])
        , ("second", InputOutput "xi" [
            mkParseSuccessResult state 'h',
            mkParseSuccessResult state 'x'])
        ]
  in testParserTrees parser cases

test_and_second :: [TestTree]
test_and_second =
  let state = OffsetStream 1 "i"
      parser = andParser empty ('x' <$ anyToken)
      cases =
        [ ("empty", InputOutput "" [mkUnlabelledStreamResult (OffsetStream 0 "") RawErrorAnyToken])
        , ("first", InputOutput "hi" [mkParseSuccessResult state 'x'])
        , ("second", InputOutput "xi" [mkParseSuccessResult state 'x'])
        ]
  in testParserTrees parser cases

test_and_all :: [TestTree]
test_and_all =
  let state = OffsetStream 1 "i"
      parser = andAllParser [matchToken 'h', 'y' <$ anyToken, matchToken 'x']
      cases =
        [ ("empty", InputOutput "" [
          mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchToken 'h' Nothing),
          mkUnlabelledStreamResult (OffsetStream 0 "") RawErrorAnyToken,
          mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchToken 'x' Nothing) ])
        , ("first", InputOutput "hi" [
          mkParseSuccessResult state 'h',
          mkParseSuccessResult state 'y',
          mkUnlabelledStreamResult (OffsetStream 0 "hi") (RawErrorMatchToken 'x' (Just 'h'))])
        , ("middle", InputOutput "zi" [
          mkUnlabelledStreamResult (OffsetStream 0 "zi") (RawErrorMatchToken 'h' (Just 'z')),
          mkParseSuccessResult state 'y',
          mkUnlabelledStreamResult (OffsetStream 0 "zi") (RawErrorMatchToken 'x' (Just 'z'))])
        , ("last", InputOutput "xi" [
          mkUnlabelledStreamResult (OffsetStream 0 "xi") (RawErrorMatchToken 'h' (Just 'x')),
          mkParseSuccessResult state 'y',
          mkParseSuccessResult state 'x'])
        ]
  in testParserTrees parser cases

test_asum :: [TestTree]
test_asum =
  let state = OffsetStream 1 "i"
      parser = asum [matchToken 'h', 'y' <$ anyToken, matchToken 'x']
      cases =
        [ ("empty", InputOutput "" [
          mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchToken 'h' Nothing),
          mkUnlabelledStreamResult (OffsetStream 0 "") RawErrorAnyToken,
          mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchToken 'x' Nothing) ])
        , ("first", InputOutput "hi" [
          mkParseSuccessResult state 'h',
          mkParseSuccessResult state 'y',
          mkUnlabelledStreamResult (OffsetStream 0 "hi") (RawErrorMatchToken 'x' (Just 'h'))])
        , ("middle", InputOutput "zi" [
          mkUnlabelledStreamResult (OffsetStream 0 "zi") (RawErrorMatchToken 'h' (Just 'z')),
          mkParseSuccessResult state 'y',
          mkUnlabelledStreamResult (OffsetStream 0 "zi") (RawErrorMatchToken 'x' (Just 'z'))])
        , ("last", InputOutput "xi" [
          mkUnlabelledStreamResult (OffsetStream 0 "xi") (RawErrorMatchToken 'h' (Just 'x')),
          mkParseSuccessResult state 'y',
          mkParseSuccessResult state 'x'])
        ]
  in testParserTrees parser cases

test_default_empty :: [TestTree]
test_default_empty =
  let parser = defaultParser 'z' empty
      cases =
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") 'z'])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") 'z'])
        ]
  in testParserTrees parser cases

test_default :: [TestTree]
test_default =
  let parser = defaultParser 'z' (matchToken 'h')
      cases =
        [ ("non-match empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") 'z'])
        , ("match", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") 'h'])
        , ("non-match", InputOutput "bye" [mkParseSuccessResult (OffsetStream 0 "bye") 'z'])
        ]
  in testParserTrees parser cases

test_bind_multi_pre :: [TestTree]
test_bind_multi_pre =
  let state = OffsetStream 1 "i"
      parser = asum ['h' <$ anyToken, matchToken 'x'] >>= \c -> pure [c, c]
      cases =
        [ ("empty", InputOutput "" [
            mkUnlabelledStreamResult (OffsetStream 0 "") RawErrorAnyToken,
            mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorMatchToken 'x' Nothing)])
        , ("first", InputOutput "hi" [
            mkParseSuccessResult state "hh",
            mkUnlabelledStreamResult (OffsetStream 0 "hi") (RawErrorMatchToken 'x' (Just 'h'))])
        , ("second", InputOutput "xi" [
            mkParseSuccessResult state "hh",
            mkParseSuccessResult state "xx"])
        ]
  in testParserTrees parser cases

test_bind_multi_post :: [TestTree]
test_bind_multi_post =
  let state1 = OffsetStream 1 "i"
      state2 = OffsetStream 2 ""
      parser = anyToken >>= \x -> asum [pure x, matchToken 'i']
      cases =
        [ ("empty", InputOutput "" [mkUnlabelledStreamResult (OffsetStream 0 "") RawErrorAnyToken])
        , ("first", InputOutput "hi" [mkParseSuccessResult state1 'h', mkParseSuccessResult state2 'i'])
        , ("second", InputOutput "xi" [mkParseSuccessResult state1 'x', mkParseSuccessResult state2 'i'])
        ]
  in testParserTrees parser cases

test_throw :: [TestTree]
test_throw =
  let err = Error "boo"
      parser = throwError err :: TestParser Int
      cases =
        [ ("empty", InputOutput "" [mkUnlabelledCustomResult (OffsetStream 0 "") err])
        , ("non-empty", InputOutput "hi" [mkUnlabelledCustomResult (OffsetStream 0 "hi") err])
        ]
  in testParserTrees parser cases

test_consume_throw :: [TestTree]
test_consume_throw =
  let err = Error "boo"
      parser = anyToken *> throwError err :: TestParser Int
      cases =
        [ ("empty", InputOutput "" [mkUnlabelledStreamResult (OffsetStream 0 "") RawErrorAnyToken])
        , ("non-empty", InputOutput "hi" [mkUnlabelledCustomResult (OffsetStream 1 "i") err])
        ]
  in testParserTrees parser cases

test_default_throw :: [TestTree]
test_default_throw =
  let err = Error "boo"
      parser = defaultParser 'z' (throwError err)
      cases =
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") 'z'])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") 'z'])
        ]
  in testParserTrees parser cases

test_default_consume_throw :: [TestTree]
test_default_consume_throw =
  let err = Error "boo"
      parser = defaultParser 'z' (anyToken *> throwError err)
      cases =
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") 'z'])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") 'z'])
        ]
  in testParserTrees parser cases

test_throw_mixed :: [TestTree]
test_throw_mixed =
  let state = OffsetStream 0 "hi"
      err = Error "boo"
      parser = asum [throwError err, pure 1 :: TestParser Int]
      cases =
        [ ("non-empty", InputOutput "hi" [mkUnlabelledCustomResult state err, mkParseSuccessResult state 1])
        ]
  in testParserTrees parser cases

test_throw_mixed_flip :: [TestTree]
test_throw_mixed_flip =
  let state = OffsetStream 0 "hi"
      err = Error "boo"
      parser = asum [pure 1 :: TestParser Int, throwError err]
      cases =
        [ ("non-empty", InputOutput "hi" [mkParseSuccessResult state 1, mkUnlabelledCustomResult state err])
        ]
  in testParserTrees parser cases

test_catch :: [TestTree]
test_catch =
  let state = OffsetStream 0 "hi"
      err = Error "boo"
      parser = catchError (asum [throwError err, pure 1]) (\(Error m) -> pure (if m == "boo" then 2 else 3)) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [mkParseSuccessResult state 2, mkParseSuccessResult state 1])
        ]
  in testParserTrees parser cases

test_catch_recur :: [TestTree]
test_catch_recur =
  let state = OffsetStream 0 "hi"
      err1 = Error "boo"
      err2 = Error "two"
      parser = catchError (throwError err1) (const (throwError err2)) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [mkUnlabelledCustomResult state err2])
        ]
  in testParserTrees parser cases

test_suppress_success :: [TestTree]
test_suppress_success =
  let state = OffsetStream 0 "hi"
      parser = suppressParser (asum [pure 1, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [mkParseSuccessResult state 1, mkParseSuccessResult state 2])
        ]
  in testParserTrees parser cases

test_suppress_fail_first :: [TestTree]
test_suppress_fail_first =
  let err = Error "boo"
      parser = suppressParser (asum [throwError err, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") 2])
        ]
  in testParserTrees parser cases

test_suppress_fail_second :: [TestTree]
test_suppress_fail_second =
  let err = Error "boo"
      parser = suppressParser (asum [pure 1, throwError err]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") 1])
        ]
  in testParserTrees parser cases

test_suppress_fail_both :: [TestTree]
test_suppress_fail_both =
  let state = OffsetStream 0 "hi"
      err1 = Error "boo1"
      err2 = Error "boo2"
      parser = suppressParser (asum [throwError err1, throwError err2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [mkUnlabelledCustomResult state err1, mkUnlabelledCustomResult state err2])
        ]
  in testParserTrees parser cases

test_isolate_success :: [TestTree]
test_isolate_success =
  let state = OffsetStream 0 "hi"
      parser = isolateParser (asum [pure 1, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [mkParseSuccessResult state 1])
        ]
  in testParserTrees parser cases

test_isolate_fail_first :: [TestTree]
test_isolate_fail_first =
  let err = Error "boo"
      parser = isolateParser (asum [throwError err, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") 2])
        ]
  in testParserTrees parser cases

test_isolate_fail_second :: [TestTree]
test_isolate_fail_second =
  let err = Error "boo"
      parser = isolateParser (asum [pure 1, throwError err]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") 1])
        ]
  in testParserTrees parser cases

test_isolate_fail_both :: [TestTree]
test_isolate_fail_both =
  let state = OffsetStream 0 "hi"
      err1 = Error "boo1"
      err2 = Error "boo2"
      parser = isolateParser (asum [throwError err1, throwError err2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [mkUnlabelledCustomResult state err1, mkUnlabelledCustomResult state err2])
        ]
  in testParserTrees parser cases

test_silence_success :: [TestTree]
test_silence_success =
  let state = OffsetStream 0 "hi"
      parser = silenceParser (asum [pure 1, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [mkParseSuccessResult state 1, mkParseSuccessResult state 2])
        ]
  in testParserTrees parser cases

test_silence_fail_first :: [TestTree]
test_silence_fail_first =
  let err = Error "boo"
      parser = silenceParser (asum [throwError err, pure 2]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") 2])
        ]
  in testParserTrees parser cases

test_silence_fail_second :: [TestTree]
test_silence_fail_second =
  let err = Error "boo"
      parser = silenceParser (asum [pure 1, throwError err]) :: TestParser Int
      cases =
        [ ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") 1])
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

test_look_ahead_pure :: [TestTree]
test_look_ahead_pure =
  let parser = lookAheadParser (pure 1) :: TestParser Int
      cases =
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") 1])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") 1])
        ]
  in testParserTrees parser cases

test_look_ahead_success :: [TestTree]
test_look_ahead_success =
  let parser = lookAheadParser anyToken
      cases =
        [ ("non-match empty", InputOutput "" [mkUnlabelledStreamResult (OffsetStream 0 "") RawErrorAnyToken])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") 'h'])
        ]
  in testParserTrees parser cases

test_look_ahead_failure :: [TestTree]
test_look_ahead_failure =
  let err = Error "boo"
      parser = lookAheadParser (anyToken *> throwError err) :: TestParser Char
      cases =
        [ ("non-match empty", InputOutput "" [mkUnlabelledStreamResult (OffsetStream 0 "") RawErrorAnyToken])
        , ("non-empty", InputOutput "hi" [mkUnlabelledCustomResult (OffsetStream 0 "hi") err])
        ]
  in testParserTrees parser cases

test_take_while :: [TestTree]
test_take_while =
  let parser = takeTokensWhile (=='h') :: TestParser Text
      cases =
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") ""])
        , ("non-match", InputOutput "i" [mkParseSuccessResult (OffsetStream 0 "i") ""])
        , ("match", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") "h"])
        , ("match 2", InputOutput "hhi" [mkParseSuccessResult (OffsetStream 2 "i") "hh"])
        , ("match end", InputOutput "hh" [mkParseSuccessResult (OffsetStream 2 "") "hh"])
        ]
  in testParserTrees parser cases

test_take_while_1 :: [TestTree]
test_take_while_1 =
  let parser = takeTokensWhile1 Nothing (=='h') :: TestParser Text
      cases =
        [ ("empty", InputOutput "" [mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorTakeTokensWhile1 Nothing Nothing)])
        , ("non-match", InputOutput "i" [mkUnlabelledStreamResult (OffsetStream 0 "i") (RawErrorTakeTokensWhile1 Nothing (Just 'i'))])
        , ("match", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") "h"])
        , ("match 2", InputOutput "hhi" [mkParseSuccessResult (OffsetStream 2 "i") "hh"])
        , ("match end", InputOutput "hh" [mkParseSuccessResult (OffsetStream 2 "") "hh"])
        ]
  in testParserTrees parser cases

test_drop_while :: [TestTree]
test_drop_while =
  let parser = dropTokensWhile (=='h') :: TestParser Int
      cases =
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") 0])
        , ("non-match", InputOutput "i" [mkParseSuccessResult (OffsetStream 0 "i") 0])
        , ("match", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") 1])
        , ("match 2", InputOutput "hhi" [mkParseSuccessResult (OffsetStream 2 "i") 2])
        , ("match end", InputOutput "hh" [mkParseSuccessResult (OffsetStream 2 "") 2])
        ]
  in testParserTrees parser cases

test_drop_while_1 :: [TestTree]
test_drop_while_1 =
  let parser = dropTokensWhile1 Nothing (=='h') :: TestParser Int
      cases =
        [ ("empty", InputOutput "" [mkUnlabelledStreamResult (OffsetStream 0 "") (RawErrorDropTokensWhile1 Nothing Nothing)])
        , ("non-match", InputOutput "i" [mkUnlabelledStreamResult (OffsetStream 0 "i") (RawErrorDropTokensWhile1 Nothing (Just 'i'))])
        , ("match", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") 1])
        , ("match 2", InputOutput "hhi" [mkParseSuccessResult (OffsetStream 2 "i") 2])
        , ("match end", InputOutput "hh" [mkParseSuccessResult (OffsetStream 2 "") 2])
        ]
  in testParserTrees parser cases

type JsonBundle = Either () (Maybe Json)

testJsonCase :: TestName -> Text -> JsonBundle -> TestTree
testJsonCase name str expected = testCase ("json " <> name) $ do
  let actual = parseJson str
  actual @?= expected

testJsonTrees :: [(TestName, Text, JsonBundle)] -> [TestTree]
testJsonTrees = fmap (\(n, s, e) -> testJsonCase n s e)

parseJson :: Text -> JsonBundle
parseJson str =
  let p = jsonParser <* matchEnd :: Parser JsonLabel Text Void Json
  in first (const ()) (runBundledParser p str)

test_json :: [TestTree]
test_json =
  let nullVal = Json JsonNull
      trueVal = Json (JsonBool True)
      falseVal = Json (JsonBool False)
      arrVal = Json . JsonArray . Seq.fromList
      strVal = Json . JsonString
      objVal = Json . JsonObject . Seq.fromList
      numVal = Json . JsonNum
      errRes = Left ()
      okRes = Right . Just
      cases =
        [ ("empty", "", errRes)
        , ("bad", "bad", errRes)
        , ("null", "null", okRes nullVal)
        , ("true", "true", okRes trueVal)
        , ("false", "false", okRes falseVal)
        , ("arr0", "[]", okRes (arrVal []))
        , ("arr1", "[null]", okRes (arrVal [nullVal]))
        , ("arr2", "[null, false]", okRes (arrVal [nullVal, falseVal]))
        , ("arr3", "[null, false, true]", okRes (arrVal [nullVal, falseVal, trueVal]))
        , ("arrx", "[null,]", errRes)
        , ("str0", "\"\"", okRes (strVal ""))
        , ("str1", "\"x\"", okRes (strVal "x"))
        , ("str2", "\"xy\"", okRes (strVal "xy"))
        , ("str3", "\"xyz\"", okRes (strVal "xyz"))
        , ("str4", "\"xy\\\"z\"", okRes (strVal "xy\"z"))
        , ("obj0", "{}", okRes (objVal []))
        , ("obj1", "{\"x\": true}", okRes (objVal [("x", trueVal)]))
        , ("obj2", "{\"x\": true, \"y\": false}", okRes (objVal [("x", trueVal), ("y", falseVal)]))
        , ("num0", "0", okRes (numVal (read "0")))
        , ("num1", "123", okRes (numVal (read "123")))
        , ("num2", "123.45", okRes (numVal (read "123.45")))
        , ("num3", "1e100", okRes (numVal (read "1e100")))
        , ("num4", "{\"x\": 1e100, \"y\": 123.45}", okRes (objVal [("x", numVal (read "1e100")), ("y", numVal (read "123.45"))]))
        ]
  in testJsonTrees cases

main :: IO ()
main = $(defaultMainGenerator)
