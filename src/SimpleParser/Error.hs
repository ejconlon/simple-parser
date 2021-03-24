module SimpleParser.Error
  ( ErrorBundle (..)
  , EmbedErrorBundle (..)
  , ErrorPos (..)
  , EmbedErrorPos (..)
  , getStreamPos
  , mapErrorStateParser
  , voidErrorParser
  , errorSpanParser
  ) where

import Control.Applicative (empty)
import Control.Monad ((>=>))
import Control.Monad.State (gets)
import Data.List.NonEmpty (NonEmpty)
import Data.Void (Void)
import SimpleParser.Parser (ParserT (..))
import SimpleParser.Result (ParseResult (..), ParseValue (..))
import SimpleParser.Stream (Span (..), StreamWithPos (..))

-- | A bundle of errors!
newtype ErrorBundle e = ErrorBundle (NonEmpty e)
  deriving (Eq, Show, Semigroup, Functor, Foldable, Traversable)

-- | Errors that can embed 'ErrorBundle'
class EmbedErrorBundle e d | d -> e where
  embedErrorBundle :: ErrorBundle e -> d

instance EmbedErrorBundle e (ErrorBundle e) where
  embedErrorBundle = id

-- | An error with position.
data ErrorPos p e = ErrorPos
  { epPos :: !p
  , epError :: !e
  } deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Errors that can embed an error with position
class EmbedErrorPos p e d | d -> p e where
  embedErrorPos :: ErrorPos p e -> d

instance EmbedErrorPos p e (ErrorPos p e) where
  embedErrorPos = id

-- | Gets the current stream position
getStreamPos :: (StreamWithPos p s, Monad m) => ParserT e s m p
getStreamPos = gets viewStreamPos

-- | Map over the error and state at error to change the error type of the parser.
mapErrorStateParser :: Monad m => (e -> s -> d) -> ParserT e s m a -> ParserT d s m a
mapErrorStateParser f p = ParserT (fmap go . runParserT p) where
  go (ParseResult v t) =
    case v of
      ParseError e -> ParseResult (ParseError (f e t)) t
      ParseSuccess a -> ParseResult (ParseSuccess a) t

-- | Drop all errors from the parse results. Unlike 'silenceParser', this
-- changes the error type of the parser so that no further errors can be thrown.
voidErrorParser :: Monad m => ParserT e s m a -> ParserT Void s m a
voidErrorParser p = ParserT (runParserT p >=> go) where
  go (ParseResult v t) =
    case v of
      ParseError _ -> empty
      ParseSuccess a -> pure (ParseResult (ParseSuccess a) t)

-- | Add spans to all errors in the given parser.
errorSpanParser :: (StreamWithPos p s, EmbedErrorPos (Span p) e d, Monad m) => ParserT e s m a -> ParserT d s m a
errorSpanParser p = do
  start <- getStreamPos
  mapErrorStateParser (\e s -> embedErrorPos (ErrorPos (Span start (viewStreamPos s)) e)) p
