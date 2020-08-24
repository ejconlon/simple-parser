{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Applicative (Alternative (..))
import Prelude
import SimpleParser
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
  expected @?= actual

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

-- test_word :: [TestTree]
-- test_word =
--   let parser = fmap toList (parseWithInput (word "hi"))
--       cases =
--         [ ("empty", InputOutput "" [])
--         , ("non-empty", InputOutput "hi" [successResult "" "hi"])
--         , ("prefix", InputOutput "hiya" [successResult "ya" "hi"])
--         , ("partial", InputOutput "hey" [])
--         , ("non-match", InputOutput "bye" [])
--         ]
--   in testParserTrees parser cases

-- test_star :: [TestTree]
-- test_star =
--   let parser = fmap toList (parseWithInput (starInput . char 'h'))
--       cases =
--         [ ("empty", InputOutput "" [successResult "" ""])
--         , ("non-empty", InputOutput "hi" [successResult "i" "h"])
--         , ("repeat", InputOutput "hhi" [successResult "i" "hh"])
--         , ("full", InputOutput "hhh" [successResult "" "hhh"])
--         , ("non-match", InputOutput "bye" [successResult "bye" ""])
--         ]
--   in testParserTrees parser cases

-- test_branch :: [TestTree]
-- test_branch =
--   let parser = branch [parseWithInput (char 'h'), parseWithInput (char 'x')]
--       cases =
--         [ ("empty", InputOutput "" [])
--         , ("first", InputOutput "hi" [successResult "i" 'h'])
--         , ("second", InputOutput "xi" [successResult "i" 'x'])
--         , ("non-match", InputOutput "bye" [])
--         ]
--   in testParserTrees parser cases

-- test_branch_first :: [TestTree]
-- test_branch_first =
--   let parser = branch [parseWithInput skip *> pure 'h', parseWithInput (char 'x')]
--       cases =
--         [ ("empty", InputOutput "" [])
--         , ("first", InputOutput "hi" [successResult "i" 'h'])
--         , ("second", InputOutput "xi" [successResult "i" 'h'])
--         ]
--   in testParserTrees parser cases

-- test_branch_second :: [TestTree]
-- test_branch_second =
--   let parser = branch [emptyParser, parseWithInput skip *> pure 'x']
--       cases =
--         [ ("empty", InputOutput "" [])
--         , ("first", InputOutput "hi" [successResult "i" 'x'])
--         , ("second", InputOutput "xi" [successResult "i" 'x'])
--         ]
--   in testParserTrees parser cases

-- test_combine :: [TestTree]
-- test_combine =
--   let parser = combine [parseWithInput (char 'h'), parseWithInput (char 'x')]
--       cases =
--         [ ("empty", InputOutput "" [])
--         , ("first", InputOutput "hi" [successResult "i" 'h'])
--         , ("second", InputOutput "xi" [successResult "i" 'x'])
--         , ("non-match", InputOutput "bye" [])
--         ]
--   in testParserTrees parser cases

-- test_combine_first :: [TestTree]
-- test_combine_first =
--   let parser = combine [parseWithInput skip *> pure 'h', parseWithInput (char 'x')]
--       cases =
--         [ ("empty", InputOutput "" [])
--         , ("first", InputOutput "hi" [successResult "i" 'h'])
--         , ("second", InputOutput "xi" [successResult "i" 'h', successResult "i" 'x'])
--         ]
--   in testParserTrees parser cases

-- test_combine_second :: [TestTree]
-- test_combine_second =
--   let parser = combine [emptyParser, parseWithInput skip *> pure 'x']
--       cases =
--         [ ("empty", InputOutput "" [])
--         , ("first", InputOutput "hi" [successResult "i" 'x'])
--         , ("second", InputOutput "xi" [successResult "i" 'x'])
--         ]
--   in testParserTrees parser cases

-- test_with_default_empty :: [TestTree]
-- test_with_default_empty =
--   let parser = withDefault 'z' emptyParser
--       cases =
--         [ ("empty", InputOutput "" [successResult "" 'z'])
--         , ("non-empty", InputOutput "hi" [successResult "hi" 'z'])
--         ]
--   in testParserTrees parser cases

-- test_with_default :: [TestTree]
-- test_with_default =
--   let parser = withDefault 'z' (parseWithInput (char 'h'))
--       cases =
--         [ ("empty", InputOutput "" [successResult "" 'z'])
--         , ("match", InputOutput "hi" [successResult "i" 'h'])
--         , ("non-match", InputOutput "bye" [successResult "bye" 'z'])
--         ]
--   in testParserTrees parser cases

-- test_look_ahead :: [TestTree]
-- test_look_ahead =
--   let parser = parseWithInput lookAhead
--       cases =
--         [ ("empty", InputOutput "" [successResult "" Nothing])
--         , ("match", InputOutput "hi" [successResult "hi" (Just 'h')])
--         ]
--   in testParserTrees parser cases

-- test_sequential :: [TestTree]
-- test_sequential =
--   let parser = fmap toList (sequential [parseWithInput (char 'h'), parseWithInput (char 'i')])
--       cases =
--         [ ("empty", InputOutput "" [])
--         , ("non-empty", InputOutput "hi" [successResult "" "hi"])
--         , ("prefix", InputOutput "hiya" [successResult "ya" "hi"])
--         , ("partial", InputOutput "hey" [])
--         , ("non-match", InputOutput "bye" [])
--         ]
--   in testParserTrees parser cases

-- test_bind_multi_pre :: [TestTree]
-- test_bind_multi_pre =
--   let parser = fmap toList (combine [parseWithInput skip *> pure 'h', parseWithInput (char 'x')] >>= \c -> pure [c, c])
--       cases =
--         [ ("empty", InputOutput "" [])
--         , ("first", InputOutput "hi" [successResult "i" "hh"])
--         , ("second", InputOutput "xi" [successResult "i" "hh", successResult "i" "xx"])
--         ]
--   in testParserTrees parser cases

-- test_bind_multi_post :: [TestTree]
-- test_bind_multi_post =
--   let parser = parseWithInput skip >>= \x -> combine [pure x, parseWithInput (char 'i')]
--       cases =
--         [ ("empty", InputOutput "" [])
--         , ("first", InputOutput "hi" [successResult "i" 'h', successResult "" 'i'])
--         , ("second", InputOutput "xi" [successResult "i" 'x', successResult "" 'i'])
--         ]
--   in testParserTrees parser cases

-- test_throw :: [TestTree]
-- test_throw =
--   let err = Error "boo"
--       parser = throwParser err :: TestParser Int
--       cases =
--         [ ("empty", InputOutput "" [failureResult "" mempty err])
--         , ("non-empty", InputOutput "hi" [failureResult "hi" mempty err])
--         ]
--   in testParserTrees parser cases

-- test_throw_named :: [TestTree]
-- test_throw_named =
--   let err = Error "boo"
--       parser = named "a" (named "b" (throwParser err)) :: TestParser Int
--       cases =
--         [ ("non-empty", InputOutput "hi" [failureResult "hi" (pathList ["a", "b"]) err])
--         ]
--   in testParserTrees parser cases

-- test_throw_named_combo :: [TestTree]
-- test_throw_named_combo =
--   let err = Error "boo"
--       parser = named "a" (combine [throwParser err, named "b" (throwParser err)]) :: TestParser Int
--       cases =
--         [ ("non-empty", InputOutput "hi" [failureResult "hi" (pathList ["a"]) err, failureResult "hi" (pathList ["a", "b"]) err])
--         ]
--   in testParserTrees parser cases

-- test_throw_mixed :: [TestTree]
-- test_throw_mixed =
--   let err = Error "boo"
--       parser = combine [throwParser err, pure 1] :: TestParser Int
--       cases =
--         [ ("non-empty", InputOutput "hi" [failureResult "hi" mempty err, successResult "hi" 1])
--         ]
--   in testParserTrees parser cases

-- test_catch :: [TestTree]
-- test_catch =
--   let err = Error "boo"
--       parser = catchParser (combine [throwParser err, pure 1]) (\(Error m) -> pure (if m == "boo" then 2 else 3)) :: TestParser Int
--       cases =
--         [ ("non-empty", InputOutput "hi" [successResult "hi" 2, successResult "hi" 1])
--         ]
--   in testParserTrees parser cases

-- test_suppress_success :: [TestTree]
-- test_suppress_success =
--   let parser = suppress (idBind (combine [pure 1, pure 2])) :: TestParser Int
--       cases =
--         [ ("non-empty", InputOutput "hi" [successResult "hi" 1, successResult "hi" 2])
--         ]
--   in testParserTrees parser cases

-- test_suppress_fail_first :: [TestTree]
-- test_suppress_fail_first =
--   let err = Error "boo"
--       parser = suppress (idBind (combine [throwParser err, pure 2])) :: TestParser Int
--       cases =
--         [ ("non-empty", InputOutput "hi" [successResult "hi" 2])
--         ]
--   in testParserTrees parser cases

-- test_suppress_fail_second :: [TestTree]
-- test_suppress_fail_second =
--   let err = Error "boo"
--       parser = suppress (idBind (combine [pure 1, throwParser err])) :: TestParser Int
--       cases =
--         [ ("non-empty", InputOutput "hi" [successResult "hi" 1])
--         ]
--   in testParserTrees parser cases

-- test_suppress_fail_both :: [TestTree]
-- test_suppress_fail_both =
--   let err1 = Error "boo1"
--       err2 = Error "boo2"
--       pathErr = BundledParseError mempty (Seq.fromList [SingleParseError "hi" mempty err1, SingleParseError "hi" mempty err2])
--       parser = suppress (idBind (combine [throwParser err1, throwParser err2])) :: TestParser Int
--       cases =
--         [ ("non-empty", InputOutput "hi" [Left pathErr])
--         ]
--   in testParserTrees parser cases

main :: IO ()
main = $(defaultMainGenerator)
