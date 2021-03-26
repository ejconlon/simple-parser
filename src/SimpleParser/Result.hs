module SimpleParser.Result
  ( StreamError (..)
  , CompoundError (..)
  , ParseError
  , ParseResult (..)
  , ParseValue (..)
  , onParseValue
  , mkParseSuccessResult
  , mkParseErrorResult
  , onParseResult
  ) where

import SimpleParser.Labels (LabelledError)

data StreamError s = StreamErrorTodo
  deriving (Eq, Show)

data CompoundError s e =
    CompoundErrorStream !(StreamError s)
  | CompoundErrorCustom !e
  deriving (Eq, Show)

type ParseError l s e = LabelledError l (CompoundError s e)

-- | Strict 'Either' for parse results.
data ParseValue l s e a =
    ParseValueError !(ParseError l s e)
  | ParseValueSuccess !a
  deriving (Eq, Show, Functor, Foldable, Traversable)

onParseValue :: (ParseError l s e -> z) -> (a -> z) -> ParseValue l s e a -> z
onParseValue onError onSuccess value =
  case value of
    ParseValueError e -> onError e
    ParseValueSuccess a -> onSuccess a

-- | Strict pair of parse result and state at the time it was yielded.
data ParseResult l s e a = ParseResult
  { prState :: !s
  , prValue :: !(ParseValue l s e a)
  } deriving (Eq, Show, Functor, Foldable, Traversable)

mkParseSuccessResult :: s -> a -> ParseResult l s e a
mkParseSuccessResult st = ParseResult st . ParseValueSuccess

mkParseErrorResult :: s -> ParseError l s e -> ParseResult l s e a
mkParseErrorResult st = ParseResult st . ParseValueError

onParseResult :: (s -> ParseError l s e -> z) -> (s -> a -> z) -> ParseResult l s e a -> z
onParseResult onError onSuccess (ParseResult st value) =
  case value of
    ParseValueError e -> onError st e
    ParseValueSuccess a -> onSuccess st a
