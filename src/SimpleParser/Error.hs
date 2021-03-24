module SimpleParser.Error
  ( ErrorBundle (..)
  , EmbedErrorBundle (..)
  , ErrorPos (..)
  , EmbedErrorPos (..)
  , getStreamPos
  , mapStateErrorParser
  , voidErrorParser
  , errorPosParser
  ) where

import Control.Applicative (empty)
import Control.Monad ((>=>))
import Control.Monad.State (gets)
import Data.List.NonEmpty (NonEmpty)
import Data.Void (Void)
import SimpleParser.Parser (ParserT (..))
import SimpleParser.Result (ParseResult (..), ParseValue (..))
import SimpleParser.Stream (Span (..), StreamWithPos (..))

newtype ErrorBundle e = ErrorBundle (NonEmpty e)
  deriving (Eq, Show, Semigroup, Functor, Foldable, Traversable)

class EmbedErrorBundle e d | d -> e where
  embedErrorBundle :: NonEmpty e -> d

instance EmbedErrorBundle e (ErrorBundle e) where
  embedErrorBundle = ErrorBundle

data ErrorPos p e = ErrorPos
  { epPos :: !p
  , epError :: !e
  } deriving (Eq, Show, Functor, Foldable, Traversable)

class EmbedErrorPos p e d | d -> p e where
  embedErrorPos :: p -> e -> d

instance EmbedErrorPos p e (ErrorPos p e) where
  embedErrorPos = ErrorPos

getStreamPos :: (StreamWithPos p s, Monad m) => ParserT e s m p
getStreamPos = gets viewStreamPos

mapStateErrorParser :: Monad m => (s -> e -> d) -> ParserT e s m a -> ParserT d s m a
mapStateErrorParser f p = ParserT (fmap go . runParserT p) where
  go (ParseResult v t) =
    case v of
      ParseError e -> ParseResult (ParseError (f t e)) t
      ParseSuccess a -> ParseResult (ParseSuccess a) t

-- | Drop all errors from the parse results. Unlike 'silenceParser', this
-- changes the error type of the parser so that no further errors can be thrown.
voidErrorParser :: Monad m => ParserT e s m a -> ParserT Void s m a
voidErrorParser p = ParserT (runParserT p >=> go) where
  go (ParseResult v t) =
    case v of
      ParseError _ -> empty
      ParseSuccess a -> pure (ParseResult (ParseSuccess a) t)

errorPosParser :: (StreamWithPos p s, EmbedErrorPos (Span p) e d, Monad m) => ParserT e s m a -> ParserT d s m a
errorPosParser p = do
  start <- getStreamPos
  mapStateErrorParser (embedErrorPos . Span start . viewStreamPos) p
