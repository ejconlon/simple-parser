module SimpleParser.Assert
  ( AssertError (..)
  , StreamAssertError
  , EmbedAssertError (..)
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

type StreamAssertError s = AssertError (Chunk s) (Token s)

class EmbedAssertError chunk token e | e -> chunk token where
  embedAssertError :: AssertError chunk token -> e

type EmbedStreamAssertError e s m = (Stream s, EmbedAssertError (Chunk s) (Token s) e, Monad m)

throwAssertError :: EmbedStreamAssertError e s m => StreamAssertError s -> ParserT e s m a
throwAssertError = throwError . embedAssertError

instance EmbedAssertError chunk token (AssertError chunk token) where
  embedAssertError = id

defaultAssertErrorParser :: EmbedStreamAssertError e s m => StreamAssertError s -> ParserT e s m a -> ParserT e s m a
defaultAssertErrorParser = defaultErrorParser . embedAssertError

assertMatchEnd :: EmbedStreamAssertError e s m => ParserT e s m ()
assertMatchEnd = peekToken >>= maybe (pure ()) (throwAssertError . AssertMatchEndError)

assertAnyToken :: EmbedStreamAssertError e s m => ParserT e s m (Token s)
assertAnyToken = defaultAssertErrorParser AssertAnyTokenError anyToken

assertAnyChunk :: EmbedStreamAssertError e s m => Int -> ParserT e s m (Chunk s)
assertAnyChunk = defaultAssertErrorParser AssertAnyChunkError . anyChunk

assertSatisfyToken :: EmbedStreamAssertError e s m => (Token s -> Bool) -> ParserT e s m (Token s)
assertSatisfyToken pcate = do
  m <- popToken
  case m of
    Just c | pcate c -> pure c
    _ -> throwAssertError (AssertSatisfyTokenError m)

assertMatchToken :: (EmbedStreamAssertError e s m, Eq (Token s)) => Token s -> ParserT e s m (Token s)
assertMatchToken t = do
  mu <- popToken
  case mu of
    Just u | t == u -> pure u
    _ -> throwAssertError (AssertMatchTokenError mu)

assertMatchChunk :: (EmbedStreamAssertError e s m, Eq (Chunk s)) => Chunk s -> ParserT e s m (Chunk s)
assertMatchChunk k = do
  mj <- popChunk (chunkLength k)
  case mj of
    Just j | k == j -> pure j
    _ -> throwAssertError (AssertMatchChunkError mj)

assertTakeTokensWhile1 :: EmbedStreamAssertError e s m => (Token s -> Bool) -> ParserT e s m (Chunk s)
assertTakeTokensWhile1 pcate = orParser (takeTokensWhile1 pcate) (peekToken >>= throwAssertError . AssertTakeTokensWhile1Error)

assertDropTokensWhile1 :: EmbedStreamAssertError e s m => (Token s -> Bool) -> ParserT e s m Int
assertDropTokensWhile1 pcate = orParser (dropTokensWhile1 pcate) (peekToken >>= throwAssertError . AssertDropTokensWhile1Error)
