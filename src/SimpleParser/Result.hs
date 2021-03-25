module SimpleParser.Result
  ( ParseResult (..)
  , ParseValue (..)
  , parseSuccessResult
  , parseErrorResult
  , parseValue
  , parseResult
  ) where

-- | Strict 'Either' for parse results.
data ParseValue e a =
    ParseError !e
  | ParseSuccess !a
  deriving (Eq, Show, Functor, Foldable, Traversable)

parseValue :: (e -> z) -> (a -> z) -> ParseValue e a -> z
parseValue onError onSuccess value =
  case value of
    ParseError e -> onError e
    ParseSuccess a -> onSuccess a

-- | Strict pair of parse result and state at the time it was yielded.
data ParseResult s e a = ParseResult
  { prState :: !s
  , prValue :: !(ParseValue e a)
  } deriving (Eq, Show, Functor, Foldable, Traversable)

parseSuccessResult :: s -> a -> ParseResult s e a
parseSuccessResult st = ParseResult st . ParseSuccess

parseErrorResult :: s -> e -> ParseResult s e a
parseErrorResult st = ParseResult st . ParseError

parseResult :: (s -> e -> z) -> (s -> a -> z) -> ParseResult s e a -> z
parseResult onError onSuccess (ParseResult st value) =
  case value of
    ParseError e -> onError st e
    ParseSuccess a -> onSuccess st a
