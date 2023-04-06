{-# LANGUAGE UndecidableInstances #-}

module SimpleParser.Result
  ( RawError (..)
  , StreamError (..)
  , coerceStreamError
  , CompoundError (..)
  , Mark (..)
  , MarkStack
  , ParseError (..)
  , parseErrorResume
  , parseErrorLabels
  , markParseError
  , unmarkParseError
  , parseErrorEnclosingLabels
  , parseErrorNarrowestSpan
  , ParseErrorBundle (..)
  , listParseErrors
  , matchSoleParseError
  , ParseSuccess (..)
  , ParseResult (..)
  )
where

import Control.Exception (Exception)
import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NESeq (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import SimpleParser.Stack (Stack (..), bottomStack, bottomUpStack, emptyStack, pushStack, topStack)
import SimpleParser.Stream (PosStream (..), Span (..), Stream (..))

data RawError chunk token
  = RawErrorMatchEnd !token
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

coerceStreamError :: (Chunk s ~ Chunk t, Token s ~ Token t) => StreamError s -> StreamError t
coerceStreamError = StreamError . unStreamError

data CompoundError s e
  = CompoundErrorStream !(StreamError s)
  | CompoundErrorFail !Text
  | CompoundErrorCustom !e
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq (Token s), Eq (Chunk s), Eq e) => Eq (CompoundError s e)

deriving instance (Show (Token s), Show (Chunk s), Show e) => Show (CompoundError s e)

data Mark l s = Mark
  { markLabel :: !(Maybe l)
  , markState :: !s
  }
  deriving (Eq, Show)

type MarkStack l s = Stack (Mark l s)

-- | Returns a sequence of labels from most general to most specific.
markStackLabels :: MarkStack l s -> Seq l
markStackLabels = bottomUpStack markLabel

data ParseError l s e = ParseError
  { peMarkStack :: !(MarkStack l s)
  , peEndState :: !s
  , peError :: !(CompoundError s e)
  }

deriving instance (Eq l, Eq s, Eq (Token s), Eq (Chunk s), Eq e) => Eq (ParseError l s e)

deriving instance (Show l, Show s, Show (Token s), Show (Chunk s), Show e) => Show (ParseError l s e)

instance
  ( Typeable l
  , Typeable s
  , Typeable (Token s)
  , Typeable (Chunk s)
  , Typeable e
  , Show l
  , Show s
  , Show (Token s)
  , Show (Chunk s)
  , Show e
  )
  => Exception (ParseError l s e)

-- | Returns the resumption point of the 'ParseError'.
-- If it has been marked, we use that, otherwise we assume it starts at the exact error point.
parseErrorResume :: ParseError l s e -> s
parseErrorResume pe = maybe (peEndState pe) markState (topStack (peMarkStack pe))

-- | Returns the sequence of ALL labels from coarsest to finest.
parseErrorLabels :: ParseError l s e -> Seq l
parseErrorLabels = markStackLabels . peMarkStack

-- | Updates a 'ParseError' with a resumption point.
markParseError :: Mark l s -> ParseError l s e -> ParseError l s e
markParseError s pe = pe {peMarkStack = pushStack s (peMarkStack pe)}

-- | Clears marks from a 'ParseError'.
unmarkParseError :: ParseError l s e -> ParseError l s e
unmarkParseError pe = pe {peMarkStack = emptyStack}

-- | Returns the narrowest span
parseErrorNarrowestSpan :: PosStream s => ParseError l s e -> (Maybe l, Span (Pos s))
parseErrorNarrowestSpan pe = (ml, Span startPos endPos)
 where
  endPos = streamViewPos (peEndState pe)
  (ml, startPos) = maybe (Nothing, endPos) (\(Mark mx s) -> (mx, streamViewPos s)) (bottomStack (peMarkStack pe))

-- | Returns labels enclosing the narrowest span, from coarsest to finest
-- Does NOT include the label for the narrowest span (if any).
parseErrorEnclosingLabels :: ParseError l s e -> Seq l
parseErrorEnclosingLabels pe =
  case unStack (peMarkStack pe) of
    Empty -> Empty
    _ :<| s -> s >>= \(Mark ml _) -> maybe Seq.empty Seq.singleton ml

data ParseSuccess s a = ParseSuccess
  { psEndState :: !s
  , psValue :: !a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype ParseErrorBundle l s e = ParseErrorBundle {unParseErrorBundle :: NESeq (ParseError l s e)}

deriving instance (Eq l, Eq s, Eq (Token s), Eq (Chunk s), Eq e) => Eq (ParseErrorBundle l s e)

deriving instance (Show l, Show s, Show (Token s), Show (Chunk s), Show e) => Show (ParseErrorBundle l s e)

instance
  ( Typeable l
  , Typeable s
  , Typeable (Token s)
  , Typeable (Chunk s)
  , Typeable e
  , Show l
  , Show s
  , Show (Token s)
  , Show (Chunk s)
  , Show e
  )
  => Exception (ParseErrorBundle l s e)

-- | Lists all errors in the bundle.
listParseErrors :: ParseErrorBundle l s e -> [ParseError l s e]
listParseErrors = toList . unParseErrorBundle

-- | If there is only one parse error in the bundle, return it, otherwise return nothing.
-- Errors can accumulate if you use unrestricted branching (with 'orParser' or 'Alternative' '<|>') or manual 'Parser' constructor application.
-- However, if you always branch with 'lookAheadMatch' then you will have singleton parse errors, and this will always return 'Just'.
matchSoleParseError :: ParseErrorBundle l s e -> Maybe (ParseError l s e)
matchSoleParseError (ParseErrorBundle es) =
  case es of
    e :<|| Empty -> Just e
    _ -> Nothing

data ParseResult l s e a
  = ParseResultError !(ParseErrorBundle l s e)
  | ParseResultSuccess !(ParseSuccess s a)
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq l, Eq s, Eq (Token s), Eq (Chunk s), Eq e, Eq a) => Eq (ParseResult l s e a)

deriving instance (Show l, Show s, Show (Token s), Show (Chunk s), Show e, Show a) => Show (ParseResult l s e a)
