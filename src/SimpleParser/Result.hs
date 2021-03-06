module SimpleParser.Result
  ( ParseResult (..)
  , ParseValue (..)
  , parseSuccessResult
  , parseErrorResult
  , parseValue
  ) where

data ParseValue e a =
    ParseError !e
  | ParseSuccess !a
  deriving (Eq, Show, Functor, Foldable, Traversable)

parseValue :: (e -> r) -> (a -> r) -> ParseValue e a -> r
parseValue onError onSuccess value =
  case value of
    ParseError e -> onError e
    ParseSuccess a -> onSuccess a

data ParseResult e s a = ParseResult
  { prValue :: !(ParseValue e a)
  , prState :: !s
  } deriving (Eq, Show, Functor, Foldable, Traversable)

parseSuccessResult :: a -> s -> ParseResult e s a
parseSuccessResult = ParseResult . ParseSuccess

parseErrorResult :: e -> s -> ParseResult e s a
parseErrorResult = ParseResult . ParseError
