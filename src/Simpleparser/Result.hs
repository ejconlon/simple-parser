module SimpleParser.Result
  ( ParseResult (..)
  , ParseValue (..)
  , parseSuccessResult
  , parseErrorResult
  ) where

data ParseValue e a =
    ParseError !e
  | ParseSuccess !a
  deriving (Eq, Show, Functor, Foldable, Traversable)

data ParseResult e s a = ParseResult
  { prValue :: !(ParseValue e a)
  , prState :: !s
  } deriving (Eq, Show, Functor, Foldable, Traversable)

parseSuccessResult :: a -> s -> ParseResult e s a
parseSuccessResult = ParseResult . ParseSuccess

parseErrorResult :: e -> s -> ParseResult e s a
parseErrorResult = ParseResult . ParseError
