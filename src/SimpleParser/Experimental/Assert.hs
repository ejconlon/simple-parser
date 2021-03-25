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

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (Reader, asks, runReader)
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

class MatchStreamAssertError s e where
  matchStreamAssertError :: e -> Maybe (StreamAssertError s)

instance MatchStreamAssertError s (StreamAssertError s) where
  matchStreamAssertError = Just

-- | Errors that embed 'StreamAssertError'
class MatchStreamAssertError s e => EmbedStreamAssertError s e where
  embedStreamAssertError :: StreamAssertError s -> e

instance EmbedStreamAssertError s (StreamAssertError s) where
  embedStreamAssertError = id

-- | Errors that use some monadic state to enrich 'StreamAssertError'
class (MatchStreamAssertError s e, Monad m) => EnrichStreamAssertError s e m where
  enrichStreamAssertError :: StreamAssertError s -> m e

instance Monad m => EnrichStreamAssertError s (StreamAssertError s) m where
  enrichStreamAssertError = pure

throwStreamAssertError :: (EnrichStreamAssertError s e (Reader r), Monad m) => StreamAssertError s -> ParserT r s e m a
throwStreamAssertError e = asks (runReader (enrichStreamAssertError e)) >>= throwError

throwAssertError :: (EnrichStreamAssertError s e (Reader r), Monad m) => AssertError (Chunk s) (Token s) -> ParserT r s e m a
throwAssertError = throwStreamAssertError . StreamAssertError

catchStreamAssertError :: (MatchStreamAssertError s e, Monad m) => ParserT r s e m a -> (StreamAssertError s -> ParserT r s e m a) -> ParserT r s e m a
catchStreamAssertError parser handler = catchError parser (\e -> maybe (throwError e) handler (matchStreamAssertError e))

-- | Contraint for parsers that can throw 'AssertError'
type ParserWithAssert r s e m = (Stream s, EnrichStreamAssertError s e (Reader r), Monad m)

-- | 'matchEnd' or throw an 'AssertError'
assertMatchEnd :: ParserWithAssert r s e m => ParserT r s e m ()
assertMatchEnd = peekToken >>= maybe (pure ()) (throwAssertError . AssertMatchEndError)

-- | 'anyToken' or throw an 'AssertError'
assertAnyToken :: ParserWithAssert r s e m => ParserT r s e m (Token s)
assertAnyToken = orParser anyToken (throwAssertError AssertAnyTokenError)

-- | 'anyChunk' or throw an 'AssertError'
assertAnyChunk :: ParserWithAssert r s e m => Int -> ParserT r s e m (Chunk s)
assertAnyChunk n = orParser (anyChunk n) (throwAssertError AssertAnyChunkError)

-- | 'satisfyToken' or throw an 'AssertError'
assertSatisfyToken :: ParserWithAssert r s e m => (Token s -> Bool) -> ParserT r s e m (Token s)
assertSatisfyToken pcate = do
  m <- popToken
  case m of
    Just c | pcate c -> pure c
    _ -> throwAssertError (AssertSatisfyTokenError m)

-- | 'matchToken' or throw an 'AssertError'
assertMatchToken :: (ParserWithAssert r s e m, Eq (Token s)) => Token s -> ParserT r s e m (Token s)
assertMatchToken t = do
  mu <- popToken
  case mu of
    Just u | t == u -> pure u
    _ -> throwAssertError (AssertMatchTokenError mu)

-- | 'matchChunk' or throw an 'AssertError'
assertMatchChunk :: (ParserWithAssert r s e m, Eq (Chunk s)) => Chunk s -> ParserT r s e m (Chunk s)
assertMatchChunk k = do
  mj <- popChunk (chunkLength k)
  case mj of
    Just j | k == j -> pure j
    _ -> throwAssertError (AssertMatchChunkError mj)

-- | 'takeTokensWhile1' or throw an 'AssertError'
assertTakeTokensWhile1 :: ParserWithAssert r s e m => (Token s -> Bool) -> ParserT r s e m (Chunk s)
assertTakeTokensWhile1 pcate = orParser (takeTokensWhile1 pcate) (peekToken >>= throwAssertError . AssertTakeTokensWhile1Error)

-- | 'dropTokensWhile1' or throw an 'AssertError'
assertDropTokensWhile1 :: ParserWithAssert r s e m => (Token s -> Bool) -> ParserT r s e m Int
assertDropTokensWhile1 pcate = orParser (dropTokensWhile1 pcate) (peekToken >>= throwAssertError . AssertDropTokensWhile1Error)
