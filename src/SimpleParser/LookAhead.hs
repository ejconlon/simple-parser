module SimpleParser.LookAhead
  ( MatchCase (..)
  , DefaultCase (..)
  , lookAheadMatch
  , MatchPos (..)
  , LookAheadTestResult (..)
  , lookAheadTest
  , lookAheadChunk
  ) where

import Control.Monad (void)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Sequence.NonEmpty as NESeq
import SimpleParser.Input (matchChunk)
import SimpleParser.Parser (ParserT (..), markParser)
import SimpleParser.Result (ParseResult (..))
import SimpleParser.Stream (Stream (..))

data MatchCase l s e m a = MatchCase
  { matchCaseLabel :: !(Maybe l)
  , matchCaseGuard :: !(ParserT l s e m ())
  , matchCaseBody :: !(ParserT l s e m a)
  }

data DefaultCase l s e m a = DefaultCase
  { defaultCaseLabel :: !(Maybe l)
  , defaultCaseBody :: !(ParserT l s e m a)
  }

data MatchBlock l s e m a = MatchBlock
  { matchBlockElems :: ![MatchCase l s e m a]
  , matchBlockDefault :: !(DefaultCase l s e m a)
  }

-- | Parse with look-ahead for each case and follow the first that matches (or follow the default if none do).
lookAheadMatch :: Monad m => MatchBlock l s e m a -> ParserT l s e m a
lookAheadMatch (MatchBlock mcs (DefaultCase dcl dcb)) = ParserT (go mcs) where
  go [] s = runParserT (markParser dcl dcb) s
  go ((MatchCase mcl mcg mcb):mcs') s = do
    mres <- runParserT mcg s
    case mres of
      Just (ParseResultSuccess _) -> runParserT (markParser mcl mcb) s
      _ -> go mcs' s

data MatchPos l = MatchPos
  { matchPosIndex :: !Int
  , matchPosLabel :: !(Maybe l)
  } deriving stock (Eq, Show)

data LookAheadTestResult l =
    LookAheadTestDefault !(Maybe l)
  | LookAheadTestMatches !(NESeq (MatchPos l))
  deriving stock (Eq, Show)

-- | Test which branches match the look-ahead. Useful to assert that your parser makes exclusive choices.
lookAheadTest :: Monad m => MatchBlock l s e m a -> s -> m (LookAheadTestResult l)
lookAheadTest (MatchBlock mcs (DefaultCase dcl _)) = go Empty 0 mcs where
  go !acc _ [] _ =
    case NESeq.nonEmptySeq acc of
      Nothing -> pure (LookAheadTestDefault dcl)
      Just ms -> pure (LookAheadTestMatches ms)
  go !acc !i ((MatchCase mcl mcg _):mcs') s = do
    mres <- runParserT mcg s
    case mres of
      Just (ParseResultSuccess _) -> go (acc :|> MatchPos i mcl) (i + 1) mcs' s
      _ -> go acc i mcs' s

-- | Simple look-ahead that matches by chunk.
lookAheadChunk :: (Stream s, Monad m, Eq (Chunk s)) => [(Chunk s, ParserT l s e m a)] -> ParserT l s e m a -> ParserT l s e m a
lookAheadChunk ps d = lookAheadMatch (MatchBlock (fmap (\(c, p) -> MatchCase Nothing (void (matchChunk c)) p) ps) (DefaultCase Nothing d))
