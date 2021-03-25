{-# LANGUAGE UndecidableInstances #-}

module SimpleParser.Experimental.Assert
  ( AssertError (..)
  , StreamAssertError (..)
  , MatchStreamAssertError (..)
  , EmbedStreamAssertError (..)
  , EnrichStreamAssertError (..)
  , throwStreamAssertError
  , catchStreamAssertError
  , ParserWithAssert
  , assertMatchEnd
  , assertAnyToken
  , assertAnyChunk
  , assertSatisfyToken
  , assertMatchToken
  , assertMatchChunk
  , assertTakeTokensWhile1
  , assertDropTokensWhile1
  ) where

import Control.Monad ((>=>))
import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans (lift)
import SimpleParser.Chunked (chunkLength)
import SimpleParser.Input (anyChunk, anyToken, dropTokensWhile1, peekToken, popChunk, popToken, takeTokensWhile1)
import SimpleParser.Parser (ParserT, orParser)
import SimpleParser.Stream (Stream (..))

-- | Failed assertions about common input functions
data AssertError chunk token =
    AssertMatchEndError !token
  | AssertAnyTokenError
  | AssertAnyChunkError
  | AssertSatisfyTokenError !(Maybe token)
  | AssertMatchTokenError !(Maybe token)
  | AssertMatchChunkError !(Maybe chunk)
  | AssertTakeTokensWhile1Error !(Maybe token)
  | AssertDropTokensWhile1Error !(Maybe token)
  deriving (Eq, Show)

-- | 'AssertError' specialized to 'Stream' types - newtyped to keep GHC happy.
newtype StreamAssertError s = StreamAssertError
  { unStreamAssertError :: AssertError (Chunk s) (Token s)
  }

instance (Eq (Token s), Eq (Chunk s)) => Eq (StreamAssertError s) where
  StreamAssertError a == StreamAssertError b = a == b

instance (Show (Token s), Show (Chunk s)) => Show (StreamAssertError s) where
  showsPrec d (StreamAssertError e) = showParen (d > 10) (showString "StreamAssertError " . showsPrec (succ d) e)

class MatchStreamAssertError e s where
  matchStreamAssertError :: e -> Maybe (StreamAssertError s)

instance MatchStreamAssertError (StreamAssertError s) s where
  matchStreamAssertError = Just

-- | Errors that embed 'StreamAssertError'
class MatchStreamAssertError e s => EmbedStreamAssertError e s where
  embedStreamAssertError :: StreamAssertError s -> e

instance EmbedStreamAssertError (StreamAssertError s) s where
  embedStreamAssertError = id

-- | Errors that use some monadic state to enrich 'StreamAssertError'
class (MatchStreamAssertError e s, Monad m) => EnrichStreamAssertError e s m where
  enrichStreamAssertError :: StreamAssertError s -> m e

instance Monad m => EnrichStreamAssertError (StreamAssertError s) s m where
  enrichStreamAssertError = pure

throwStreamAssertError :: EnrichStreamAssertError e s m => StreamAssertError s -> ParserT e s m a
throwStreamAssertError = (lift . enrichStreamAssertError) >=> throwError

throwAssertError :: EnrichStreamAssertError e s m => AssertError (Chunk s) (Token s) -> ParserT e s m a
throwAssertError = throwStreamAssertError . StreamAssertError

catchStreamAssertError :: (MatchStreamAssertError e s, Monad m) => ParserT e s m a -> (StreamAssertError s -> ParserT e s m a) -> ParserT e s m a
catchStreamAssertError parser handler = catchError parser (\e -> maybe (throwError e) handler (matchStreamAssertError e))

-- | Contraint for parsers that can throw 'AssertError'
type ParserWithAssert e s m = (Stream s, EnrichStreamAssertError e s m)

-- | 'matchEnd' or throw an 'AssertError'
assertMatchEnd :: ParserWithAssert e s m => ParserT e s m ()
assertMatchEnd = peekToken >>= maybe (pure ()) (throwAssertError . AssertMatchEndError)

-- | 'anyToken' or throw an 'AssertError'
assertAnyToken :: ParserWithAssert e s m => ParserT e s m (Token s)
assertAnyToken = orParser anyToken (throwAssertError AssertAnyTokenError)

-- | 'anyChunk' or throw an 'AssertError'
assertAnyChunk :: ParserWithAssert e s m => Int -> ParserT e s m (Chunk s)
assertAnyChunk n = orParser (anyChunk n) (throwAssertError AssertAnyChunkError)

-- | 'satisfyToken' or throw an 'AssertError'
assertSatisfyToken :: ParserWithAssert e s m => (Token s -> Bool) -> ParserT e s m (Token s)
assertSatisfyToken pcate = do
  m <- popToken
  case m of
    Just c | pcate c -> pure c
    _ -> throwAssertError (AssertSatisfyTokenError m)

-- | 'matchToken' or throw an 'AssertError'
assertMatchToken :: (ParserWithAssert e s m, Eq (Token s)) => Token s -> ParserT e s m (Token s)
assertMatchToken t = do
  mu <- popToken
  case mu of
    Just u | t == u -> pure u
    _ -> throwAssertError (AssertMatchTokenError mu)

-- | 'matchChunk' or throw an 'AssertError'
assertMatchChunk :: (ParserWithAssert e s m, Eq (Chunk s)) => Chunk s -> ParserT e s m (Chunk s)
assertMatchChunk k = do
  mj <- popChunk (chunkLength k)
  case mj of
    Just j | k == j -> pure j
    _ -> throwAssertError (AssertMatchChunkError mj)

-- | 'takeTokensWhile1' or throw an 'AssertError'
assertTakeTokensWhile1 :: ParserWithAssert e s m => (Token s -> Bool) -> ParserT e s m (Chunk s)
assertTakeTokensWhile1 pcate = orParser (takeTokensWhile1 pcate) (peekToken >>= throwAssertError . AssertTakeTokensWhile1Error)

-- | 'dropTokensWhile1' or throw an 'AssertError'
assertDropTokensWhile1 :: ParserWithAssert e s m => (Token s -> Bool) -> ParserT e s m Int
assertDropTokensWhile1 pcate = orParser (dropTokensWhile1 pcate) (peekToken >>= throwAssertError . AssertDropTokensWhile1Error)
