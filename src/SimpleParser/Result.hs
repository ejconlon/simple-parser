{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module SimpleParser.Result
  ( RawError (..)
  , StreamError (..)
  , CompoundError (..)
  , ParseError (..)
  , ParseResult (..)
  , ParseValue (..)
  , onParseValue
  , mkParseSuccessResult
  , mkParseErrorResult
  , onParseResult
  ) where

import SimpleParser.Labels (HasLabelStack (..), LabelStack)
import SimpleParser.Stream (Stream (..))

data RawError label chunk token =
    RawErrorMatchEnd !token
  | RawErrorAnyToken
  | RawErrorAnyChunk
  | RawErrorSatisfyToken !(Maybe label) !(Maybe token)
  | RawErrorMatchToken !token !(Maybe token)
  | RawErrorMatchChunk !chunk !(Maybe chunk)
  | RawErrorTakeTokensWhile1 !(Maybe label) !(Maybe token)
  | RawErrorDropTokensWhile1 !(Maybe label) !(Maybe token)
  deriving (Eq, Show)

-- | 'RawStreamError' specialized to 'Stream' types - newtyped to allow GHC
-- to derive eq/show in the absense of type families.
newtype StreamError l s = StreamError
  { unStreamError :: RawError l (Chunk s) (Token s)
  }

deriving instance (Eq l, Eq (Token s), Eq (Chunk s)) => Eq (StreamError l s)
deriving instance (Show l, Show (Token s), Show (Chunk s)) => Show (StreamError l s)

data CompoundError l s e =
    CompoundErrorStream !(StreamError l s)
  | CompoundErrorCustom !e
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq l, Eq (Token s), Eq (Chunk s), Eq e) => Eq (CompoundError l s e)
deriving instance (Show l, Show (Token s), Show (Chunk s), Show e) => Show (CompoundError l s e)

data ParseError l s e = ParseError
  { peLabels :: !(LabelStack l)
  , peEndState :: !s
  , peError :: !(CompoundError l s e)
  }

deriving instance (Eq l, Eq s, Eq (Token s), Eq (Chunk s), Eq e) => Eq (ParseError l s e)
deriving instance (Show l, Show s, Show (Token s), Show (Chunk s), Show e) => Show (ParseError l s e)

instance HasLabelStack l (ParseError l s e) where
  viewLabelStack = peLabels
  setLabelStack ls pe = pe { peLabels = ls }

-- | Strict 'Either' for parse results.
data ParseValue l s e a =
    ParseValueError !(ParseError l s e)
  | ParseValueSuccess !a
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq l, Eq s, Eq (Token s), Eq (Chunk s), Eq e, Eq a) => Eq (ParseValue l s e a)
deriving instance (Show l, Show s, Show (Token s), Show (Chunk s), Show e, Show a) => Show (ParseValue l s e a)

onParseValue :: (ParseError l s e -> z) -> (a -> z) -> ParseValue l s e a -> z
onParseValue onError onSuccess value =
  case value of
    ParseValueError e -> onError e
    ParseValueSuccess a -> onSuccess a

-- | Strict pair of parse result and state at the time it was yielded.
data ParseResult l s e a = ParseResult
  { prState :: !s
  , prValue :: !(ParseValue l s e a)
  } deriving (Functor, Foldable, Traversable)

deriving instance (Eq l, Eq s, Eq (Token s), Eq (Chunk s), Eq e, Eq a) => Eq (ParseResult l s e a)
deriving instance (Show l, Show s, Show (Token s), Show (Chunk s), Show e, Show a) => Show (ParseResult l s e a)

mkParseSuccessResult :: s -> a -> ParseResult l s e a
mkParseSuccessResult st = ParseResult st . ParseValueSuccess

mkParseErrorResult :: s -> ParseError l s e -> ParseResult l s e a
mkParseErrorResult st = ParseResult st . ParseValueError

onParseResult :: (s -> ParseError l s e -> z) -> (s -> a -> z) -> ParseResult l s e a -> z
onParseResult onError onSuccess (ParseResult st value) =
  case value of
    ParseValueError e -> onError st e
    ParseValueSuccess a -> onSuccess st a
