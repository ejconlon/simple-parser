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
import qualified Data.Text as T
import Data.Void (Void)
import SimpleParser
import SimpleParser.Examples.Json (Json (..), JsonF (..), JsonLabel, jsonParser)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.TH (defaultMainGenerator)

newtype Label = Label { unLabel :: String } deriving (Eq, Show, IsString)

newtype Error = Error { unError :: String } deriving (Eq, Show, IsString)

type TestState = OffsetStream Text

type TestParser a = Parser Label TestState Error a

type TestResult a = ParseResult Label TestState Error a

type TestRawError = RawError Label Text Char

type TestParseError = ParseError Label TestState Error

data ParserCase a = ParserCase !TestName !(TestParser a) !Text ![TestResult a]

sucRes :: TestState -> a -> TestResult a
sucRes = mkParseSuccessResult

errRes :: TestState -> TestParseError -> TestResult a
errRes = mkParseErrorResult

custErr :: TestState -> Error -> TestParseError
custErr es = ParseError emptyLabelStack es . CompoundErrorCustom

stmErr :: TestState -> TestRawError -> TestParseError
stmErr es = ParseError emptyLabelStack es . CompoundErrorStream . StreamError

custErrRes :: TestState -> TestState -> Error -> TestResult a
custErrRes s es = errRes s . custErr es

stmErrRes :: TestState -> TestState -> TestRawError -> TestResult a
stmErrRes s es = errRes s . stmErr es

anyTokErrRes :: TestState -> TestResult a
anyTokErrRes s = stmErrRes s s RawErrorAnyToken

anyChunkErrRes :: TestState -> TestResult a
anyChunkErrRes s = stmErrRes s s RawErrorAnyChunk

fwd :: Int -> TestState -> TestState
fwd n (OffsetStream i t) =
  let m = min n (T.length t)
  in OffsetStream (i + m) (T.drop m t)

matchTokErrRes :: TestState -> Char -> Maybe Char -> TestResult a
matchTokErrRes s x my = stmErrRes s (fwd 1 s) (RawErrorMatchToken x my)

matchChunkErrRes :: TestState -> Text -> Maybe Text -> TestResult a
matchChunkErrRes s x my = stmErrRes s (fwd (T.length x) s) (RawErrorMatchChunk x my)

matchEndErrRes :: TestState -> Char -> TestResult a
matchEndErrRes s x = stmErrRes s (fwd 1 s) (RawErrorMatchEnd x)

takeTokErrRes :: TestState -> Int -> Maybe Char -> TestResult a
takeTokErrRes s n my = stmErrRes s (fwd n s) (RawErrorTakeTokensWhile1 Nothing my)

dropTokErrRes :: TestState -> Int -> Maybe Char -> TestResult a
dropTokErrRes s n my = stmErrRes s (fwd n s) (RawErrorDropTokensWhile1 Nothing my)

testParserCase :: (Show a, Eq a) => ParserCase a -> TestTree
testParserCase (ParserCase name parser input expected) = testCase name $ do
  let actual = runParser parser emptyLabelStack (newOffsetStream input)
  actual @?= expected

test_empty :: [TestTree]
test_empty =
  let parser = empty :: TestParser Int
      cases =
        [ ParserCase "empty" parser "" []
        , ParserCase "non-empty" parser "hi" []
        ]
  in fmap testParserCase cases

test_pure :: [TestTree]
test_pure =
  let parser = pure (1 :: Int)
      cases =
        [ ParserCase "empty" parser "" [sucRes (OffsetStream 0 "") 1]
        , ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 0 "hi") 1]
        ]
  in fmap testParserCase cases

test_peek_token :: [TestTree]
test_peek_token =
  let parser = peekToken
      cases =
        [ ParserCase "empty" parser "" [sucRes (OffsetStream 0 "") Nothing]
        , ParserCase "match" parser "hi" [sucRes (OffsetStream 0 "hi") (Just 'h')]
        ]
  in fmap testParserCase cases

test_pop_token :: [TestTree]
test_pop_token =
  let parser = popToken
      cases =
        [ ParserCase "empty" parser "" [sucRes (OffsetStream 0 "") Nothing]
        , ParserCase "match" parser "hi" [sucRes (OffsetStream 1 "i") (Just 'h')]
        ]
  in fmap testParserCase cases

test_peek_chunk :: [TestTree]
test_peek_chunk =
  let parser = peekChunk 2
      cases =
        [ ParserCase "len 0" parser "" [sucRes (OffsetStream 0 "") Nothing]
        , ParserCase "len 1" parser "h" [sucRes (OffsetStream 0 "h") (Just "h")]
        , ParserCase "len 2" parser "hi" [sucRes (OffsetStream 0 "hi") (Just "hi")]
        , ParserCase "len 3" parser "hii" [sucRes (OffsetStream 0 "hii") (Just "hi")]
        ]
  in fmap testParserCase cases

test_pop_chunk :: [TestTree]
test_pop_chunk =
  let parser = popChunk 2
      cases =
        [ ParserCase "len 0" parser "" [sucRes (OffsetStream 0 "") Nothing]
        , ParserCase "len 1" parser "h" [sucRes (OffsetStream 1 "") (Just "h")]
        , ParserCase "len 2" parser "hi" [sucRes (OffsetStream 2 "") (Just "hi")]
        , ParserCase "len 3" parser "hii" [sucRes (OffsetStream 2 "i") (Just "hi")]
        ]
  in fmap testParserCase cases

test_drop_chunk :: [TestTree]
test_drop_chunk =
  let parser = dropChunk 2
      cases =
        [ ParserCase "len 0" parser "" [sucRes (OffsetStream 0 "") Nothing]
        , ParserCase "len 1" parser "h" [sucRes (OffsetStream 1 "") (Just 1)]
        , ParserCase "len 2" parser "hi" [sucRes (OffsetStream 2 "") (Just 2)]
        , ParserCase "len 3" parser "hii" [sucRes (OffsetStream 2 "i") (Just 2)]
        ]
  in fmap testParserCase cases

test_is_end :: [TestTree]
test_is_end =
  let parser = isEnd
      cases =
        [ ParserCase "empty" parser "" [sucRes (OffsetStream 0 "") True]
        , ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 0 "hi") False]
        ]
  in fmap testParserCase cases

test_match_end :: [TestTree]
test_match_end =
  let parser = matchEnd
      cases =
        [ ParserCase "empty" parser "" [sucRes (OffsetStream 0 "") ()]
        , ParserCase "non-empty" parser "hi" [matchEndErrRes (OffsetStream 0 "hi") 'h']
        ]
  in fmap testParserCase cases

test_any_token :: [TestTree]
test_any_token =
  let parser = anyToken
      cases =
        [ ParserCase "empty" parser "" [anyTokErrRes (OffsetStream 0 "")]
        , ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 1 "i") 'h']
        ]
  in fmap testParserCase cases

test_any_chunk :: [TestTree]
test_any_chunk =
  let parser = anyChunk 2 :: TestParser Text
      cases =
        [ ParserCase "len 0" parser "" [anyChunkErrRes (OffsetStream 0 "")]
        , ParserCase "len 1" parser "h" [sucRes (OffsetStream 1 "") "h"]
        , ParserCase "len 2" parser "hi" [sucRes (OffsetStream 2 "") "hi"]
        , ParserCase "len 3" parser "hii" [sucRes (OffsetStream 2 "i") "hi"]
        ]
  in fmap testParserCase cases

test_match_token :: [TestTree]
test_match_token =
  let parser = matchToken 'h'
      cases =
        [ ParserCase "empty" parser "" [matchTokErrRes (OffsetStream 0 "") 'h' Nothing]
        , ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 1 "i") 'h']
        , ParserCase "non-match" parser "bye" [matchTokErrRes (OffsetStream 0 "bye") 'h' (Just 'b')]
        ]
  in fmap testParserCase cases

test_match_chunk :: [TestTree]
test_match_chunk =
  let parser = matchChunk "hi"
      cases =
        [ ParserCase "empty" parser "" [matchChunkErrRes (OffsetStream 0 "") "hi" Nothing]
        , ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 2 "") "hi"]
        , ParserCase "prefix" parser "hiya" [sucRes (OffsetStream 2 "ya") "hi"]
        , ParserCase "partial" parser "hey" [matchChunkErrRes (OffsetStream 0 "hey") "hi" (Just "he")]
        , ParserCase "non-match" parser "bye" [matchChunkErrRes (OffsetStream 0 "bye") "hi" (Just "by")]
        , ParserCase "short" parser "h" [matchChunkErrRes (OffsetStream 0 "h") "hi" (Just "h")]
        ]
  in fmap testParserCase cases

test_greedy_star :: [TestTree]
test_greedy_star =
  let parser = greedyStarParser (matchToken 'h') :: TestParser String
      cases =
        [ ParserCase "empty" parser "" [sucRes (OffsetStream 0 "") ""]
        , ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 1 "i") "h"]
        , ParserCase "repeat" parser "hhi" [sucRes (OffsetStream 2 "i") "hh"]
        , ParserCase "full" parser "hhh" [sucRes (OffsetStream 3 "") "hhh"]
        , ParserCase "non-match" parser "bye" [sucRes (OffsetStream 0 "bye") ""]
        ]
  in fmap testParserCase cases

test_greedy_star_unit :: [TestTree]
test_greedy_star_unit =
  let parser = greedyStarParser_ (matchToken 'h')
      cases =
        [ ParserCase "empty" parser "" [sucRes (OffsetStream 0 "") ()]
        , ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 1 "i") ()]
        , ParserCase "repeat" parser "hhi" [sucRes (OffsetStream 2 "i") ()]
        , ParserCase "full" parser "hhh" [sucRes (OffsetStream 3 "") ()]
        , ParserCase "non-match" parser "bye" [sucRes (OffsetStream 0 "bye") ()]
        ]
  in fmap testParserCase cases

test_greedy_plus :: [TestTree]
test_greedy_plus =
  let parser = greedyPlusParser (matchToken 'h') :: TestParser String
      cases =
        [ ParserCase "empty" parser "" [matchTokErrRes (OffsetStream 0 "") 'h' Nothing]
        , ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 1 "i") "h"]
        , ParserCase "repeat" parser "hhi" [sucRes (OffsetStream 2 "i") "hh"]
        , ParserCase "full" parser "hhh" [sucRes (OffsetStream 3 "") "hhh"]
        , ParserCase "non-match" parser "bye" [matchTokErrRes (OffsetStream 0 "bye") 'h' (Just 'b')]
        ]
  in fmap testParserCase cases

test_greedy_plus_unit :: [TestTree]
test_greedy_plus_unit =
  let parser = greedyPlusParser_ (matchToken 'h')
      cases =
        [ ParserCase "empty" parser "" [matchTokErrRes (OffsetStream 0 "") 'h' Nothing]
        , ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 1 "i") ()]
        , ParserCase "repeat" parser "hhi" [sucRes (OffsetStream 2 "i") ()]
        , ParserCase "full" parser "hhh" [sucRes (OffsetStream 3 "") ()]
        , ParserCase "non-match" parser "bye" [matchTokErrRes (OffsetStream 0 "bye") 'h' (Just 'b')]
        ]
  in fmap testParserCase cases

test_or :: [TestTree]
test_or =
  let parser = orParser (matchToken 'h') (matchToken 'x')
      cases =
        [ ParserCase "empty" parser ""
            [ matchTokErrRes (OffsetStream 0 "") 'h' Nothing
            , matchTokErrRes (OffsetStream 0 "") 'x' Nothing
            ]
        , ParserCase "first" parser "hi" [sucRes (OffsetStream 1 "i") 'h']
        , ParserCase "second" parser "xi" [sucRes (OffsetStream 1 "i") 'x']
        , ParserCase "non-match" parser "bye"
            [ matchTokErrRes (OffsetStream 0 "bye") 'h' (Just 'b')
            , matchTokErrRes (OffsetStream 0 "bye") 'x' (Just 'b')
            ]
        ]
  in fmap testParserCase cases

test_or_all :: [TestTree]
test_or_all =
  let state = OffsetStream 1 "i"
      parser = orAllParser [matchToken 'h', 'y' <$ anyToken, matchToken 'x']
      cases =
        [ ParserCase "empty" parser ""
            [ matchTokErrRes (OffsetStream 0 "") 'h' Nothing
            , anyTokErrRes (OffsetStream 0 "")
            , matchTokErrRes (OffsetStream 0 "") 'x' Nothing
            ]
        , ParserCase "first" parser "hi" [sucRes state 'h']
        , ParserCase "middle" parser "zi" [sucRes state 'y']
        , ParserCase "last" parser "xi" [sucRes state 'y']
        ]
  in fmap testParserCase cases

test_and :: [TestTree]
test_and =
  let parser = andParser (matchToken 'h') (matchToken 'x')
      cases =
        [ ParserCase "empty" parser ""
            [ matchTokErrRes (OffsetStream 0 "") 'h' Nothing
            , matchTokErrRes (OffsetStream 0 "") 'x' Nothing
            ]
        , ParserCase "first" parser "hi"
            [ sucRes (OffsetStream 1 "i") 'h'
            , matchTokErrRes (OffsetStream 0 "hi") 'x' (Just 'h')
            ]
        , ParserCase "second" parser "xi"
            [ matchTokErrRes (OffsetStream 0 "xi") 'h' (Just 'x')
            , sucRes (OffsetStream 1 "i") 'x'
            ]
        , ParserCase "non-match" parser "bye"
            [ matchTokErrRes (OffsetStream 0 "bye") 'h' (Just 'b')
            , matchTokErrRes (OffsetStream 0 "bye") 'x' (Just 'b')
            ]
        ]
  in fmap testParserCase cases

test_and_first :: [TestTree]
test_and_first =
  let state = OffsetStream 1 "i"
      parser = andParser ('h' <$ anyToken) (matchToken 'x')
      cases =
        [ ParserCase "empty" parser ""
            [ anyTokErrRes (OffsetStream 0 "")
            , matchTokErrRes (OffsetStream 0 "") 'x' Nothing
            ]
        , ParserCase "first" parser "hi"
            [ sucRes state 'h'
            , matchTokErrRes (OffsetStream 0 "hi") 'x' (Just 'h')
            ]
        , ParserCase "second" parser "xi"
            [ sucRes state 'h'
            , sucRes state 'x'
            ]
        ]
  in fmap testParserCase cases

test_and_second :: [TestTree]
test_and_second =
  let state = OffsetStream 1 "i"
      parser = andParser empty ('x' <$ anyToken)
      cases =
        [ ParserCase "empty" parser "" [anyTokErrRes (OffsetStream 0 "")]
        , ParserCase "first" parser "hi" [sucRes state 'x']
        , ParserCase "second" parser "xi" [sucRes state 'x']
        ]
  in fmap testParserCase cases

test_and_all :: [TestTree]
test_and_all =
  let state = OffsetStream 1 "i"
      parser = andAllParser [matchToken 'h', 'y' <$ anyToken, matchToken 'x']
      cases =
        [ ParserCase "empty" parser ""
            [ matchTokErrRes (OffsetStream 0 "") 'h' Nothing
            , anyTokErrRes (OffsetStream 0 "")
            , matchTokErrRes (OffsetStream 0 "") 'x' Nothing
            ]
        , ParserCase "first" parser "hi"
            [ sucRes state 'h'
            , sucRes state 'y'
            , matchTokErrRes (OffsetStream 0 "hi") 'x' (Just 'h')
            ]
        , ParserCase "middle" parser "zi"
            [ matchTokErrRes (OffsetStream 0 "zi") 'h' (Just 'z')
            , sucRes state 'y'
            , matchTokErrRes (OffsetStream 0 "zi") 'x' (Just 'z')
            ]
        , ParserCase "last" parser "xi"
            [ matchTokErrRes (OffsetStream 0 "xi") 'h' (Just 'x')
            , sucRes state 'y'
            , sucRes state 'x'
            ]
        ]
  in fmap testParserCase cases

test_asum :: [TestTree]
test_asum =
  let state = OffsetStream 1 "i"
      parser = asum [matchToken 'h', 'y' <$ anyToken, matchToken 'x']
      cases =
        [ ParserCase "empty" parser ""
            [ matchTokErrRes (OffsetStream 0 "") 'h' Nothing
            , anyTokErrRes (OffsetStream 0 "")
            , matchTokErrRes (OffsetStream 0 "") 'x' Nothing
            ]
        , ParserCase "first" parser "hi"
            [ sucRes state 'h'
            , sucRes state 'y'
            , matchTokErrRes (OffsetStream 0 "hi") 'x' (Just 'h')
            ]
        , ParserCase "middle" parser "zi"
            [ matchTokErrRes (OffsetStream 0 "zi") 'h' (Just 'z')
            , sucRes state 'y'
            , matchTokErrRes (OffsetStream 0 "zi") 'x' (Just 'z')
            ]
        , ParserCase "last" parser "xi"
            [ matchTokErrRes (OffsetStream 0 "xi") 'h' (Just 'x')
            , sucRes state 'y'
            , sucRes state 'x'
            ]
        ]
  in fmap testParserCase cases

test_default_empty :: [TestTree]
test_default_empty =
  let parser = defaultParser 'z' empty
      cases =
        [ ParserCase "empty" parser "" [sucRes (OffsetStream 0 "") 'z']
        , ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 0 "hi") 'z']
        ]
  in fmap testParserCase cases

test_default :: [TestTree]
test_default =
  let parser = defaultParser 'z' (matchToken 'h')
      cases =
        [ ParserCase "non-match empty" parser "" [sucRes (OffsetStream 0 "") 'z']
        , ParserCase "match" parser "hi" [sucRes (OffsetStream 1 "i") 'h']
        , ParserCase "non-match" parser "bye" [sucRes (OffsetStream 0 "bye") 'z']
        ]
  in fmap testParserCase cases

test_bind_multi_pre :: [TestTree]
test_bind_multi_pre =
  let state = OffsetStream 1 "i"
      parser = asum ['h' <$ anyToken, matchToken 'x'] >>= \c -> pure [c, c]
      cases =
        [ ParserCase "empty" parser ""
            [ anyTokErrRes (OffsetStream 0 "")
            , matchTokErrRes (OffsetStream 0 "") 'x' Nothing
            ]
        , ParserCase "first" parser "hi"
            [ sucRes state "hh"
            , matchTokErrRes (OffsetStream 0 "hi") 'x' (Just 'h')
            ]
        , ParserCase "second" parser "xi"
            [ sucRes state "hh"
            , sucRes state "xx"
            ]
        ]
  in fmap testParserCase cases

test_bind_multi_post :: [TestTree]
test_bind_multi_post =
  let state1 = OffsetStream 1 "i"
      state2 = OffsetStream 2 ""
      parser = anyToken >>= \x -> asum [pure x, matchToken 'i']
      cases =
        [ ParserCase "empty" parser "" [anyTokErrRes (OffsetStream 0 "")]
        , ParserCase "first" parser "hi" [sucRes state1 'h', sucRes state2 'i']
        , ParserCase "second" parser "xi" [sucRes state1 'x', sucRes state2 'i']
        ]
  in fmap testParserCase cases

test_throw :: [TestTree]
test_throw =
  let err = Error "boo"
      parser = throwError err :: TestParser Int
      cases =
        [ ParserCase "empty" parser "" [custErrRes (OffsetStream 0 "") (OffsetStream 0 "") err]
        , ParserCase "non-empty" parser "hi" [custErrRes (OffsetStream 0 "hi") (OffsetStream 0 "hi") err]
        ]
  in fmap testParserCase cases

test_consume_throw :: [TestTree]
test_consume_throw =
  let err = Error "boo"
      parser = anyToken *> throwError err :: TestParser Int
      cases =
        [ ParserCase "empty" parser "" [anyTokErrRes (OffsetStream 0 "")]
        , ParserCase "non-empty" parser "hi" [custErrRes (OffsetStream 1 "i") (OffsetStream 1 "i") err]
        ]
  in fmap testParserCase cases

test_default_throw :: [TestTree]
test_default_throw =
  let err = Error "boo"
      parser = defaultParser 'z' (throwError err)
      cases =
        [ ParserCase "empty" parser "" [sucRes (OffsetStream 0 "") 'z']
        , ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 0 "hi") 'z']
        ]
  in fmap testParserCase cases

test_default_consume_throw :: [TestTree]
test_default_consume_throw =
  let err = Error "boo"
      parser = defaultParser 'z' (anyToken *> throwError err)
      cases =
        [ ParserCase "empty" parser "" [sucRes (OffsetStream 0 "") 'z']
        , ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 0 "hi") 'z']
        ]
  in fmap testParserCase cases

test_throw_mixed :: [TestTree]
test_throw_mixed =
  let state = OffsetStream 0 "hi"
      err = Error "boo"
      parser = asum [throwError err, pure 1 :: TestParser Int]
      cases =
        [ ParserCase "non-empty" parser "hi" [custErrRes state state err, sucRes state 1]
        ]
  in fmap testParserCase cases

test_throw_mixed_flip :: [TestTree]
test_throw_mixed_flip =
  let state = OffsetStream 0 "hi"
      err = Error "boo"
      parser = asum [pure 1 :: TestParser Int, throwError err]
      cases =
        [ ParserCase "non-empty" parser "hi" [sucRes state 1, custErrRes state state err]
        ]
  in fmap testParserCase cases

test_catch :: [TestTree]
test_catch =
  let state = OffsetStream 0 "hi"
      err = Error "boo"
      parser = catchError (asum [throwError err, pure 1]) (\(Error m) -> pure (if m == "boo" then 2 else 3)) :: TestParser Int
      cases =
        [ ParserCase "non-empty" parser "hi" [sucRes state 2, sucRes state 1]
        ]
  in fmap testParserCase cases

test_catch_recur :: [TestTree]
test_catch_recur =
  let state = OffsetStream 0 "hi"
      err1 = Error "boo"
      err2 = Error "two"
      parser = catchError (throwError err1) (const (throwError err2)) :: TestParser Int
      cases =
        [ ParserCase "non-empty" parser "hi" [custErrRes state state err2]
        ]
  in fmap testParserCase cases

test_suppress_success :: [TestTree]
test_suppress_success =
  let state = OffsetStream 0 "hi"
      parser = suppressParser (asum [pure 1, pure 2]) :: TestParser Int
      cases =
        [ ParserCase "non-empty" parser "hi" [sucRes state 1, sucRes state 2]
        ]
  in fmap testParserCase cases

test_suppress_fail_first :: [TestTree]
test_suppress_fail_first =
  let err = Error "boo"
      parser = suppressParser (asum [throwError err, pure 2]) :: TestParser Int
      cases =
        [ ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 0 "hi") 2]
        ]
  in fmap testParserCase cases

test_suppress_fail_second :: [TestTree]
test_suppress_fail_second =
  let err = Error "boo"
      parser = suppressParser (asum [pure 1, throwError err]) :: TestParser Int
      cases =
        [ ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 0 "hi") 1]
        ]
  in fmap testParserCase cases

test_suppress_fail_both :: [TestTree]
test_suppress_fail_both =
  let state = OffsetStream 0 "hi"
      err1 = Error "boo1"
      err2 = Error "boo2"
      parser = suppressParser (asum [throwError err1, throwError err2]) :: TestParser Int
      cases =
        [ ParserCase "non-empty" parser "hi" [custErrRes state state err1, custErrRes state state err2]
        ]
  in fmap testParserCase cases

test_isolate_success :: [TestTree]
test_isolate_success =
  let state = OffsetStream 0 "hi"
      parser = isolateParser (asum [pure 1, pure 2]) :: TestParser Int
      cases =
        [ ParserCase "non-empty" parser "hi" [sucRes state 1]
        ]
  in fmap testParserCase cases

test_isolate_fail_first :: [TestTree]
test_isolate_fail_first =
  let err = Error "boo"
      parser = isolateParser (asum [throwError err, pure 2]) :: TestParser Int
      cases =
        [ ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 0 "hi") 2]
        ]
  in fmap testParserCase cases

test_isolate_fail_second :: [TestTree]
test_isolate_fail_second =
  let err = Error "boo"
      parser = isolateParser (asum [pure 1, throwError err]) :: TestParser Int
      cases =
        [ ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 0 "hi") 1]
        ]
  in fmap testParserCase cases

test_isolate_fail_both :: [TestTree]
test_isolate_fail_both =
  let state = OffsetStream 0 "hi"
      err1 = Error "boo1"
      err2 = Error "boo2"
      parser = isolateParser (asum [throwError err1, throwError err2]) :: TestParser Int
      cases =
        [ ParserCase "non-empty" parser "hi" [custErrRes state state err1, custErrRes state state err2]
        ]
  in fmap testParserCase cases

test_silence_success :: [TestTree]
test_silence_success =
  let state = OffsetStream 0 "hi"
      parser = silenceParser (asum [pure 1, pure 2]) :: TestParser Int
      cases =
        [ ParserCase "non-empty" parser "hi" [sucRes state 1, sucRes state 2]
        ]
  in fmap testParserCase cases

test_silence_fail_first :: [TestTree]
test_silence_fail_first =
  let err = Error "boo"
      parser = silenceParser (asum [throwError err, pure 2]) :: TestParser Int
      cases =
        [ ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 0 "hi") 2]
        ]
  in fmap testParserCase cases

test_silence_fail_second :: [TestTree]
test_silence_fail_second =
  let err = Error "boo"
      parser = silenceParser (asum [pure 1, throwError err]) :: TestParser Int
      cases =
        [ ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 0 "hi") 1]
        ]
  in fmap testParserCase cases

test_silence_fail_both :: [TestTree]
test_silence_fail_both =
  let err1 = Error "boo1"
      err2 = Error "boo2"
      parser = silenceParser (asum [throwError err1, throwError err2]) :: TestParser Int
      cases =
        [ ParserCase "non-empty" parser "hi" []
        ]
  in fmap testParserCase cases

test_look_ahead_pure :: [TestTree]
test_look_ahead_pure =
  let parser = lookAheadParser (pure 1) :: TestParser Int
      cases =
        [ ParserCase "empty" parser "" [sucRes (OffsetStream 0 "") 1]
        , ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 0 "hi") 1]
        ]
  in fmap testParserCase cases

test_look_ahead_success :: [TestTree]
test_look_ahead_success =
  let parser = lookAheadParser anyToken
      cases =
        [ ParserCase "non-match empty" parser "" [anyTokErrRes (OffsetStream 0 "")]
        , ParserCase "non-empty" parser "hi" [sucRes (OffsetStream 0 "hi") 'h']
        ]
  in fmap testParserCase cases

test_look_ahead_failure :: [TestTree]
test_look_ahead_failure =
  let err = Error "boo"
      parser = lookAheadParser (anyToken *> throwError err) :: TestParser Char
      cases =
        [ ParserCase "non-match empty" parser "" [anyTokErrRes (OffsetStream 0 "")]
        , ParserCase "non-empty" parser "hi" [custErrRes (OffsetStream 0 "hi") (OffsetStream 1 "i") err]
        ]
  in fmap testParserCase cases

test_take_while :: [TestTree]
test_take_while =
  let parser = takeTokensWhile (=='h') :: TestParser Text
      cases =
        [ ParserCase "empty" parser "" [sucRes (OffsetStream 0 "") ""]
        , ParserCase "non-match" parser "i" [sucRes (OffsetStream 0 "i") ""]
        , ParserCase "match" parser "hi" [sucRes (OffsetStream 1 "i") "h"]
        , ParserCase "match 2" parser "hhi" [sucRes (OffsetStream 2 "i") "hh"]
        , ParserCase "match end" parser "hh" [sucRes (OffsetStream 2 "") "hh"]
        ]
  in fmap testParserCase cases

test_take_while_1 :: [TestTree]
test_take_while_1 =
  let parser = takeTokensWhile1 Nothing (=='h') :: TestParser Text
      cases =
        [ ParserCase "empty" parser "" [takeTokErrRes(OffsetStream 0 "") 0 Nothing]
        , ParserCase "non-match" parser "i" [takeTokErrRes (OffsetStream 0 "i") 0 (Just 'i')]
        , ParserCase "match" parser "hi" [sucRes (OffsetStream 1 "i") "h"]
        , ParserCase "match 2" parser "hhi" [sucRes (OffsetStream 2 "i") "hh"]
        , ParserCase "match end" parser "hh" [sucRes (OffsetStream 2 "") "hh"]
        ]
  in fmap testParserCase cases

test_drop_while :: [TestTree]
test_drop_while =
  let parser = dropTokensWhile (=='h') :: TestParser Int
      cases =
        [ ParserCase "empty" parser "" [sucRes (OffsetStream 0 "") 0]
        , ParserCase "non-match" parser "i" [sucRes (OffsetStream 0 "i") 0]
        , ParserCase "match" parser "hi" [sucRes (OffsetStream 1 "i") 1]
        , ParserCase "match 2" parser "hhi" [sucRes (OffsetStream 2 "i") 2]
        , ParserCase "match end" parser "hh" [sucRes (OffsetStream 2 "") 2]
        ]
  in fmap testParserCase cases

test_drop_while_1 :: [TestTree]
test_drop_while_1 =
  let parser = dropTokensWhile1 Nothing (=='h') :: TestParser Int
      cases =
        [ ParserCase "empty" parser "" [dropTokErrRes (OffsetStream 0 "") 0 Nothing]
        , ParserCase "non-match" parser "i" [dropTokErrRes (OffsetStream 0 "i") 0 (Just 'i')]
        , ParserCase "match" parser "hi" [sucRes (OffsetStream 1 "i") 1]
        , ParserCase "match 2" parser "hhi" [sucRes (OffsetStream 2 "i") 2]
        , ParserCase "match end" parser "hh" [sucRes (OffsetStream 2 "") 2]
        ]
  in fmap testParserCase cases

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
      jerrRes = Left ()
      jsucRes = Right . Just
      cases =
        [ ("empty", "", jerrRes)
        , ("bad", "bad", jerrRes)
        , ("null", "null", jsucRes nullVal)
        , ("true", "true", jsucRes trueVal)
        , ("false", "false", jsucRes falseVal)
        , ("arr0", "[]", jsucRes (arrVal []))
        , ("arr1", "[null]", jsucRes (arrVal [nullVal]))
        , ("arr2", "[null, false]", jsucRes (arrVal [nullVal, falseVal]))
        , ("arr3", "[null, false, true]", jsucRes (arrVal [nullVal, falseVal, trueVal]))
        , ("arrx", "[null,]", jerrRes)
        , ("str0", "\"\"", jsucRes (strVal ""))
        , ("str1", "\"x\"", jsucRes (strVal "x"))
        , ("str2", "\"xy\"", jsucRes (strVal "xy"))
        , ("str3", "\"xyz\"", jsucRes (strVal "xyz"))
        , ("str4", "\"xy\\\"z\"", jsucRes (strVal "xy\"z"))
        , ("obj0", "{}", jsucRes (objVal []))
        , ("obj1", "{\"x\": true}", jsucRes (objVal [("x", trueVal)]))
        , ("obj2", "{\"x\": true, \"y\": false}", jsucRes (objVal [("x", trueVal), ("y", falseVal)]))
        , ("num0", "0", jsucRes (numVal (read "0")))
        , ("num1", "123", jsucRes (numVal (read "123")))
        , ("num2", "123.45", jsucRes (numVal (read "123.45")))
        , ("num3", "1e100", jsucRes (numVal (read "1e100")))
        , ("num4", "{\"x\": 1e100, \"y\": 123.45}", jsucRes (objVal [("x", numVal (read "1e100")), ("y", numVal (read "123.45"))]))
        ]
  in testJsonTrees cases

main :: IO ()
main = $(defaultMainGenerator)
