{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Applicative (empty)
import Control.Monad.Except (catchError, throwError)
import Data.Foldable (asum)
import Data.Functor (($>))
import qualified Data.Sequence as Seq
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import SimpleParser
import SimpleParser.Examples.Json (Json (..), JsonContext, JsonF (..), jsonParser)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.TH (defaultMainGenerator)

newtype Label = Label { unLabel :: String } deriving (Eq, Show, IsString)

newtype Error = Error { unError :: String } deriving (Eq, Show, IsString)

type TestState = OffsetStream Text

type TestParser a = Parser Label TestState Error a

type TestResult a = ParseResult Label TestState Error a

data InputOutput a = InputOutput !Text ![TestResult a]

unlabelledCustomError :: Error -> ParseError Label TestState Error
unlabelledCustomError = unlabelledError . CompoundErrorCustom

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
        , ("non-empty", InputOutput "hi" [])
        ]
  in testParserTrees parser cases

test_any_token :: [TestTree]
test_any_token =
  let parser = anyToken
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") 'h'])
        ]
  in testParserTrees parser cases

test_any_chunk :: [TestTree]
test_any_chunk =
  let parser = anyChunk 2 :: TestParser Text
      cases =
        [ ("len 0", InputOutput "" [])
        , ("len 1", InputOutput "h" [mkParseSuccessResult (OffsetStream 1 "") "h"])
        , ("len 2", InputOutput "hi" [mkParseSuccessResult (OffsetStream 2 "") "hi"])
        , ("len 3", InputOutput "hii" [mkParseSuccessResult (OffsetStream 2 "i") "hi"])
        ]
  in testParserTrees parser cases

test_match_token :: [TestTree]
test_match_token =
  let parser = matchToken 'h'
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") 'h'])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_match_chunk :: [TestTree]
test_match_chunk =
  let parser = matchChunk "hi"
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 2 "") "hi"])
        , ("prefix", InputOutput "hiya" [mkParseSuccessResult (OffsetStream 2 "ya") "hi"])
        , ("partial", InputOutput "hey" [])
        , ("non-match", InputOutput "bye" [])
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
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") "h"])
        , ("repeat", InputOutput "hhi" [mkParseSuccessResult (OffsetStream 2 "i") "hh"])
        , ("full", InputOutput "hhh" [mkParseSuccessResult (OffsetStream 3 "") "hhh"])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_greedy_plus_unit :: [TestTree]
test_greedy_plus_unit =
  let parser = greedyPlusParser_ (matchToken 'h')
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") ()])
        , ("repeat", InputOutput "hhi" [mkParseSuccessResult (OffsetStream 2 "i") ()])
        , ("full", InputOutput "hhh" [mkParseSuccessResult (OffsetStream 3 "") ()])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_or :: [TestTree]
test_or =
  let parser = orParser (matchToken 'h') (matchToken 'x')
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") 'h'])
        , ("second", InputOutput "xi" [mkParseSuccessResult (OffsetStream 1 "i") 'x'])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_or_first :: [TestTree]
test_or_first =
  let parser = orParser (anyToken $> 'h') (matchToken 'x')
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") 'h'])
        , ("second", InputOutput "xi" [mkParseSuccessResult (OffsetStream 1 "i") 'h'])
        ]
  in testParserTrees parser cases

test_or_all :: [TestTree]
test_or_all =
  let state = OffsetStream 1 "i"
      parser = orAllParser [matchToken 'h', anyToken $> 'y', matchToken 'x']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [mkParseSuccessResult state 'h'])
        , ("middle", InputOutput "zi" [mkParseSuccessResult state 'y'])
        , ("last", InputOutput "xi" [mkParseSuccessResult state 'y'])
        ]
  in testParserTrees parser cases

test_and :: [TestTree]
test_and =
  let parser = andParser (matchToken 'h') (matchToken 'x')
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") 'h'])
        , ("second", InputOutput "xi" [mkParseSuccessResult (OffsetStream 1 "i") 'x'])
        , ("non-match", InputOutput "bye" [])
        ]
  in testParserTrees parser cases

test_and_first :: [TestTree]
test_and_first =
  let state = OffsetStream 1 "i"
      parser = andParser (anyToken $> 'h') (matchToken 'x')
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [mkParseSuccessResult state 'h'])
        , ("second", InputOutput "xi" [mkParseSuccessResult state 'h', mkParseSuccessResult state 'x'])
        ]
  in testParserTrees parser cases

test_and_second :: [TestTree]
test_and_second =
  let state = OffsetStream 1 "i"
      parser = andParser empty (anyToken $> 'x')
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [mkParseSuccessResult state 'x'])
        , ("second", InputOutput "xi" [mkParseSuccessResult state 'x'])
        ]
  in testParserTrees parser cases

test_and_all :: [TestTree]
test_and_all =
  let state = OffsetStream 1 "i"
      parser = andAllParser [matchToken 'h', anyToken $> 'y', matchToken 'x']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [mkParseSuccessResult state 'h', mkParseSuccessResult state 'y'])
        , ("middle", InputOutput "zi" [mkParseSuccessResult state 'y'])
        , ("last", InputOutput "xi" [mkParseSuccessResult state 'y', mkParseSuccessResult state 'x'])
        ]
  in testParserTrees parser cases

test_asum :: [TestTree]
test_asum =
  let state = OffsetStream 1 "i"
      parser = asum [matchToken 'h', anyToken $> 'y', matchToken 'x']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [mkParseSuccessResult state 'h', mkParseSuccessResult state 'y'])
        , ("middle", InputOutput "zi" [mkParseSuccessResult state 'y'])
        , ("last", InputOutput "xi" [mkParseSuccessResult state 'y', mkParseSuccessResult state 'x'])
        ]
  in testParserTrees parser cases

test_with_default_empty :: [TestTree]
test_with_default_empty =
  let parser = defaultSuccessParser 'z' empty
      cases =
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") 'z'])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") 'z'])
        ]
  in testParserTrees parser cases

test_with_default :: [TestTree]
test_with_default =
  let parser = defaultSuccessParser 'z' (matchToken 'h')
      cases =
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") 'z'])
        , ("match", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") 'h'])
        , ("non-match", InputOutput "bye" [mkParseSuccessResult (OffsetStream 0 "bye") 'z'])
        ]
  in testParserTrees parser cases

test_bind_multi_pre :: [TestTree]
test_bind_multi_pre =
  let state = OffsetStream 1 "i"
      parser = asum [anyToken $> 'h', matchToken 'x'] >>= \c -> pure [c, c]
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [mkParseSuccessResult state "hh"])
        , ("second", InputOutput "xi" [mkParseSuccessResult state "hh", mkParseSuccessResult state "xx"])
        ]
  in testParserTrees parser cases

test_bind_multi_post :: [TestTree]
test_bind_multi_post =
  let state1 = OffsetStream 1 "i"
      state2 = OffsetStream 2 ""
      parser = anyToken >>= \x -> asum [pure x, matchToken 'i']
      cases =
        [ ("empty", InputOutput "" [])
        , ("first", InputOutput "hi" [mkParseSuccessResult state1 'h', mkParseSuccessResult state2 'i'])
        , ("second", InputOutput "xi" [mkParseSuccessResult state1 'x', mkParseSuccessResult state2 'i'])
        ]
  in testParserTrees parser cases

test_throw :: [TestTree]
test_throw =
  let err = Error "boo"
      parser = throwError err :: TestParser Int
      cases =
        [ ("empty", InputOutput "" [mkParseErrorResult (OffsetStream 0 "") (unlabelledCustomError err)])
        , ("non-empty", InputOutput "hi" [mkParseErrorResult (OffsetStream 0 "hi") (unlabelledCustomError err)])
        ]
  in testParserTrees parser cases

test_consume_throw :: [TestTree]
test_consume_throw =
  let err = Error "boo"
      parser = anyToken *> throwError err :: TestParser Int
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [mkParseErrorResult (OffsetStream 1 "i") (unlabelledCustomError err)])
        ]
  in testParserTrees parser cases

test_with_default_throw :: [TestTree]
test_with_default_throw =
  let err = Error "boo"
      parser = defaultSuccessParser 'z' (throwError err)
      cases =
        [ ("empty", InputOutput "" [mkParseErrorResult (OffsetStream 0 "") (unlabelledCustomError err)])
        , ("non-empty", InputOutput "hi" [mkParseErrorResult (OffsetStream 0 "hi") (unlabelledCustomError err)])
        ]
  in testParserTrees parser cases

test_with_default_consume_throw :: [TestTree]
test_with_default_consume_throw =
  let err = Error "boo"
      parser = defaultSuccessParser 'z' (anyToken *> throwError err)
      cases =
        [ ("empty", InputOutput "" [mkParseSuccessResult (OffsetStream 0 "") 'z'])
        , ("non-empty", InputOutput "hi" [mkParseErrorResult (OffsetStream 1 "i") (unlabelledCustomError err)])
        ]
  in testParserTrees parser cases

test_throw_mixed :: [TestTree]
test_throw_mixed =
  let state = OffsetStream 0 "hi"
      err = Error "boo"
      parser = asum [throwError err, pure 1 :: TestParser Int]
      cases =
        [ ("non-empty", InputOutput "hi" [mkParseErrorResult state (unlabelledCustomError err), mkParseSuccessResult state 1])
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
        [ ("non-empty", InputOutput "hi" [mkParseErrorResult state (unlabelledCustomError err2)])
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
        [ ("non-empty", InputOutput "hi" [mkParseErrorResult state (unlabelledCustomError err1), mkParseErrorResult state (unlabelledCustomError err2)])
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
        [ ("non-empty", InputOutput "hi" [mkParseErrorResult state (unlabelledCustomError err1), mkParseErrorResult state (unlabelledCustomError err2)])
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

test_look_ahead_success :: [TestTree]
test_look_ahead_success =
  let parser = lookAheadParser anyToken
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [mkParseSuccessResult (OffsetStream 0 "hi") 'h'])
        ]
  in testParserTrees parser cases

test_look_ahead_failure :: [TestTree]
test_look_ahead_failure =
  let err = Error "boo"
      parser = lookAheadParser (anyToken *> throwError err) :: TestParser Char
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-empty", InputOutput "hi" [mkParseErrorResult (OffsetStream 0 "hi") (unlabelledCustomError err)])
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
  let parser = takeTokensWhile1 (=='h') :: TestParser Text
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-match", InputOutput "i" [])
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
  let parser = dropTokensWhile1 (=='h') :: TestParser Int
      cases =
        [ ("empty", InputOutput "" [])
        , ("non-match", InputOutput "i" [])
        , ("match", InputOutput "hi" [mkParseSuccessResult (OffsetStream 1 "i") 1])
        , ("match 2", InputOutput "hhi" [mkParseSuccessResult (OffsetStream 2 "i") 2])
        , ("match end", InputOutput "hh" [mkParseSuccessResult (OffsetStream 2 "") 2])
        ]
  in testParserTrees parser cases

testJsonCase :: TestName -> Text -> Maybe Json -> TestTree
testJsonCase name str expected = testCase ("json " <> name) $ do
  let actual = parseJson str
  actual @?= expected

testJsonTrees :: [(TestName, Text, Maybe Json)] -> [TestTree]
testJsonTrees = fmap (\(n, s, e) -> testJsonCase n s e)

parseJson :: Text -> Maybe Json
parseJson str =
  let p = jsonParser <* matchEnd :: Parser JsonContext Text Void Json
  in runVoidParser p str

test_json :: [TestTree]
test_json =
  let nullVal = Json JsonNull
      trueVal = Json (JsonBool True)
      falseVal = Json (JsonBool False)
      arrVal = Json . JsonArray . Seq.fromList
      strVal = Json . JsonString
      objVal = Json . JsonObject . Seq.fromList
      numVal = Json . JsonNum
      cases =
        [ ("empty", "", Nothing)
        , ("bad", "bad", Nothing)
        , ("null", "null", Just nullVal)
        , ("true", "true", Just trueVal)
        , ("false", "false", Just falseVal)
        , ("arr0", "[]", Just (arrVal []))
        , ("arr1", "[null]", Just (arrVal [nullVal]))
        , ("arr2", "[null, false]", Just (arrVal [nullVal, falseVal]))
        , ("arr3", "[null, false, true]", Just (arrVal [nullVal, falseVal, trueVal]))
        , ("arrx", "[null,]", Nothing)
        , ("str0", "\"\"", Just (strVal ""))
        , ("str1", "\"x\"", Just (strVal "x"))
        , ("str2", "\"xy\"", Just (strVal "xy"))
        , ("str3", "\"xyz\"", Just (strVal "xyz"))
        , ("str4", "\"xy\\\"z\"", Just (strVal "xy\"z"))
        , ("obj0", "{}", Just (objVal []))
        , ("obj1", "{\"x\": true}", Just (objVal [("x", trueVal)]))
        , ("obj2", "{\"x\": true, \"y\": false}", Just (objVal [("x", trueVal), ("y", falseVal)]))
        , ("num0", "0", Just (numVal (read "0")))
        , ("num1", "123", Just (numVal (read "123")))
        , ("num2", "123.45", Just (numVal (read "123.45")))
        , ("num3", "1e100", Just (numVal (read "1e100")))
        , ("num4", "{\"x\": 1e100, \"y\": 123.45}", Just (objVal [("x", numVal (read "1e100")), ("y", numVal (read "123.45"))]))
        ]
  in testJsonTrees cases

main :: IO ()
main = $(defaultMainGenerator)
