{-# LANGUAGE UndecidableInstances #-}

module SimpleParser.Result
  ( RawError (..)
  , StreamError (..)
  , CompoundError (..)
  , Mark (..)
  , ParseError (..)
  , parseErrorResume
  , markParseError
  , unmarkParseError
  , parseErrorEnclosingLabels
  , parseErrorNarrowestSpan
  , ParseSuccess (..)
  , ParseResult (..)
  , matchSoleParseError
  ) where

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NESeq (..))
import Data.Text (Text)
import SimpleParser.Stack (Stack (..), bottomStack, emptyStack, pushStack, topStack)
import SimpleParser.Stream (PosStream (..), Span (..), Stream (..))

data RawError chunk token =
    RawErrorMatchEnd !token
  | RawErrorAnyToken
  | RawErrorAnyChunk
  | RawErrorSatisfyToken !(Maybe token)
  | RawErrorMatchToken !token !(Maybe token)
  | RawErrorMatchChunk !chunk !(Maybe chunk)
  | RawErrorTakeTokensWhile1 !(Maybe token)
  | RawErrorDropTokensWhile1 !(Maybe token)
  deriving (Eq, Show)

-- | 'RawStreamError' specialized to 'Stream' types - newtyped to allow GHC
-- to derive eq/show in the absense of type families.
newtype StreamError s = StreamError
  { unStreamError :: RawError (Chunk s) (Token s)
  }

deriving instance (Eq (Token s), Eq (Chunk s)) => Eq (StreamError s)
deriving instance (Show (Token s), Show (Chunk s)) => Show (StreamError s)

data CompoundError s e =
    CompoundErrorStream !(StreamError s)
  | CompoundErrorFail !Text
  | CompoundErrorCustom !e
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq (Token s), Eq (Chunk s), Eq e) => Eq (CompoundError s e)
deriving instance (Show (Token s), Show (Chunk s), Show e) => Show (CompoundError s e)

data Mark l s = Mark
  { markLabel :: !(Maybe l)
  , markState :: !s
  } deriving (Eq, Show)

type MarkStack l s = Stack (Mark l s)

data ParseError l s e = ParseError
  { peMarkStack :: !(MarkStack l s)
  , peEndState :: !s
  , peError :: !(CompoundError s e)
  }

-- | Returns the resumption point of the 'ParseError'.
-- If it has been marked, we use that, otherwise we assume it starts at the exact error point.
parseErrorResume :: ParseError l s e -> s
parseErrorResume pe = maybe (peEndState pe) markState (topStack (peMarkStack pe))

-- | Updates a 'ParseError' with a resumption point.
markParseError :: Mark l s -> ParseError l s e -> ParseError l s e
markParseError s pe = pe { peMarkStack = pushStack s (peMarkStack pe) }

-- | Clears marks from a 'ParseError'.
unmarkParseError :: ParseError l s e -> ParseError l s e
unmarkParseError pe = pe { peMarkStack = emptyStack }

-- | Returns the narrowest span
parseErrorNarrowestSpan :: PosStream s => ParseError l s e -> (Maybe l, Span (Pos s))
parseErrorNarrowestSpan pe = (ml, Span startPos endPos) where
  endPos = streamViewPos (peEndState pe)
  (ml, startPos) = maybe (Nothing, endPos) (\(Mark mx s) -> (mx, streamViewPos s)) (bottomStack (peMarkStack pe))

-- | Returns labels enclosing the narrowest span, from coarsest to finest
parseErrorEnclosingLabels :: ParseError l s e -> Seq l
parseErrorEnclosingLabels pe =
  case unStack (peMarkStack pe) of
    Empty -> Empty
    _ :<| s -> s >>= \(Mark ml _) -> maybe Seq.empty Seq.singleton ml

deriving instance (Eq l, Eq s, Eq (Token s), Eq (Chunk s), Eq e) => Eq (ParseError l s e)
deriving instance (Show l, Show s, Show (Token s), Show (Chunk s), Show e) => Show (ParseError l s e)

data ParseSuccess s a = ParseSuccess
  { psEndState :: !s
  , psValue :: !a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data ParseResult l s e a =
    ParseResultError !(NESeq (ParseError l s e))
  | ParseResultSuccess !(ParseSuccess s a)
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq l, Eq s, Eq (Token s), Eq (Chunk s), Eq e, Eq a) => Eq (ParseResult l s e a)
deriving instance (Show l, Show s, Show (Token s), Show (Chunk s), Show e, Show a) => Show (ParseResult l s e a)

-- | If there is one parse error, return it, otherwise return nothing.
-- Errors can accumulate if you use unrestricted branching (with 'orParser' or 'Alternative' '<|>') or manual 'Parser' constructor application.
-- However, if you always branch with 'lookAheadMatch' then you will have singleton parse errors, and this will always return 'Just'.
matchSoleParseError :: NESeq (ParseError l s e) -> Maybe (ParseError l s e)
matchSoleParseError es =
  case es of
    e :<|| Empty -> Just e
    _ -> Nothing
