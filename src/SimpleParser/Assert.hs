module SimpleParser.Assert
  ( AssertError (..)
  , StreamAssertError
  , EmbedAssertError (..)
  , EmbedStreamAssertError
  , throwAssertError
  , assertMatchEnd
  , assertAnyToken
  , assertAnyChunk
  , assertSatisfyToken
  , assertMatchToken
  , assertMatchChunk
  , assertTakeTokensWhile1
  , assertDropTokensWhile1
  ) where

import Control.Monad.Except (throwError)
import SimpleParser.Input (anyChunk, anyToken, dropTokensWhile1, peekToken, popChunk, popToken, takeTokensWhile1)
import SimpleParser.Parser (ParserT, defaultErrorParser, orParser)
import SimpleParser.Stream (Stream (..), chunkLength)

-- | Failed assertions about common input functions.
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

-- | 'AssertError' specialized to 'Stream' types.
type StreamAssertError s = AssertError (Chunk s) (Token s)

-- | Errors that embed 'AssertError'
class EmbedAssertError chunk token e | e -> chunk token where
  embedAssertError :: AssertError chunk token -> e

-- | Contraint for parsers that can throw 'AssertError'
type EmbedStreamAssertError e s m = (Stream s, EmbedAssertError (Chunk s) (Token s) e, Monad m)

throwAssertError :: EmbedStreamAssertError e s m => StreamAssertError s -> ParserT e s m a
throwAssertError = throwError . embedAssertError

instance EmbedAssertError chunk token (AssertError chunk token) where
  embedAssertError = id

defaultAssertErrorParser :: EmbedStreamAssertError e s m => StreamAssertError s -> ParserT e s m a -> ParserT e s m a
defaultAssertErrorParser = defaultErrorParser . embedAssertError

-- | 'matchEnd' or throw an 'AssertError'
assertMatchEnd :: EmbedStreamAssertError e s m => ParserT e s m ()
assertMatchEnd = peekToken >>= maybe (pure ()) (throwAssertError . AssertMatchEndError)

-- | 'anyToken' or throw an 'AssertError'
assertAnyToken :: EmbedStreamAssertError e s m => ParserT e s m (Token s)
assertAnyToken = defaultAssertErrorParser AssertAnyTokenError anyToken

-- | 'anyChunk' or throw an 'AssertError'
assertAnyChunk :: EmbedStreamAssertError e s m => Int -> ParserT e s m (Chunk s)
assertAnyChunk = defaultAssertErrorParser AssertAnyChunkError . anyChunk

-- | 'satisfyToken' or throw an 'AssertError'
assertSatisfyToken :: EmbedStreamAssertError e s m => (Token s -> Bool) -> ParserT e s m (Token s)
assertSatisfyToken pcate = do
  m <- popToken
  case m of
    Just c | pcate c -> pure c
    _ -> throwAssertError (AssertSatisfyTokenError m)

-- | 'matchToken' or throw an 'AssertError'
assertMatchToken :: (EmbedStreamAssertError e s m, Eq (Token s)) => Token s -> ParserT e s m (Token s)
assertMatchToken t = do
  mu <- popToken
  case mu of
    Just u | t == u -> pure u
    _ -> throwAssertError (AssertMatchTokenError mu)

-- | 'matchChunk' or throw an 'AssertError'
assertMatchChunk :: (EmbedStreamAssertError e s m, Eq (Chunk s)) => Chunk s -> ParserT e s m (Chunk s)
assertMatchChunk k = do
  mj <- popChunk (chunkLength k)
  case mj of
    Just j | k == j -> pure j
    _ -> throwAssertError (AssertMatchChunkError mj)

-- | 'takeTokensWhile1' or throw an 'AssertError'
assertTakeTokensWhile1 :: EmbedStreamAssertError e s m => (Token s -> Bool) -> ParserT e s m (Chunk s)
assertTakeTokensWhile1 pcate = orParser (takeTokensWhile1 pcate) (peekToken >>= throwAssertError . AssertTakeTokensWhile1Error)

-- | 'dropTokensWhile1' or throw an 'AssertError'
assertDropTokensWhile1 :: EmbedStreamAssertError e s m => (Token s -> Bool) -> ParserT e s m Int
assertDropTokensWhile1 pcate = orParser (dropTokensWhile1 pcate) (peekToken >>= throwAssertError . AssertDropTokensWhile1Error)
