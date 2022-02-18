{-# LANGUAGE UndecidableInstances #-}

module SimpleParser.LookAhead
  ( MatchCase (..)
  , PureMatchCase
  , MatchBlock (..)
  , PureMatchBlock
  , lookAheadMatch
  , consumeMatch
  , MatchPos (..)
  , LookAheadTestResult (..)
  , lookAheadTest
  , pureLookAheadTest
  , lookAheadSimple
  ) where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Sequence.NonEmpty as NESeq
import SimpleParser.Parser (ParserT (..), lookAheadParser, markParser)
import SimpleParser.Result (ParseResult (..), ParseSuccess (..))

data MatchCase l s e m a b = MatchCase
  { matchCaseLabel :: !(Maybe l)
  , matchCaseChoose :: !(a -> Bool)
  , matchCaseHandle :: !(ParserT l s e m b)
  }

type PureMatchCase l s e a b = MatchCase l s e Identity a b

data MatchBlock l s e m a b = MatchBlock
  { matchBlockSelect :: !(ParserT l s e m a)
  , matchBlockDefault :: !(ParserT l s e m b)
  , matchBlockElems :: ![MatchCase l s e m a b]
  }

type PureMatchBlock l s e a b = MatchBlock l s e Identity a b

-- | Parse with look-ahead for each case and follow the first that matches (or follow the default if none do).
lookAheadMatch :: Monad m => MatchBlock l s e m a b -> ParserT l s e m b
lookAheadMatch (MatchBlock sel dc mcs) = lookAheadParser sel >>= go mcs where
  go [] _ = dc
  go ((MatchCase mcl mcg mch):mcs') val =
    if mcg val
      then markParser mcl mch
      else go mcs' val

-- | Same as 'lookAheadMatch' but consumes the selector instead of looking ahead.
-- Cases will not have to re-parse the selected portion.
consumeMatch :: Monad m => MatchBlock l s e m a b -> ParserT l s e m b
consumeMatch (MatchBlock sel dc mcs) = sel >>= go mcs where
  go [] _ = dc
  go ((MatchCase mcl mcg mch):mcs') val =
    if mcg val
      then markParser mcl mch
      else go mcs' val

data MatchPos l = MatchPos
  { matchPosIndex :: !Int
  , matchPosLabel :: !(Maybe l)
  } deriving stock (Eq, Show)

data LookAheadTestResult l =
    LookAheadTestEmpty
  | LookAheadTestDefault
  | LookAheadTestMatches !(NESeq (MatchPos l))
  deriving stock (Eq, Show)

-- | Test which branches match the look-ahead. Useful to assert that your parser makes exclusive choices.
lookAheadTest :: Monad m => MatchBlock l s e m a b -> s -> m (LookAheadTestResult l)
lookAheadTest (MatchBlock sel _ mcs) = go1 where
  go1 s = do
    mres <- runParserT sel s
    case mres of
      Just (ParseResultSuccess (ParseSuccess _ val)) -> pure (go2 Empty 0 mcs val)
      _ -> pure LookAheadTestEmpty
  go2 !acc _ [] _ = maybe LookAheadTestDefault LookAheadTestMatches (NESeq.nonEmptySeq acc)
  go2 !acc !i ((MatchCase mcl mcg _):mcs') val =
    if mcg val
      then go2 (acc :|> MatchPos i mcl) (i + 1) mcs' val
      else go2 acc (i + 1) mcs' val

pureLookAheadTest :: PureMatchBlock l s e a b -> s -> LookAheadTestResult l
pureLookAheadTest mb = runIdentity . lookAheadTest mb

-- | Simple look-ahead that selects a parser based on first equal prefix.
lookAheadSimple :: (Monad m, Eq a) => ParserT l s e m a -> ParserT l s e m b -> [(a, ParserT l s e m b)] -> ParserT l s e m b
lookAheadSimple sel dc pairs = lookAheadMatch (MatchBlock sel dc mcs) where
  mcs = [MatchCase Nothing (== x) p | (x, p) <- pairs]
