module SimpleParser.Input
  ( peekToken
  , popToken
  , peekChunk
  , popChunk
  , dropChunk
  , isEnd
  , matchEnd
  , anyToken
  , anyChunk
  , satisfyToken
  , foldTokensWhile
  , takeTokensWhile
  , dropTokensWhile
  , matchToken
  , matchChunk
  ) where

import Control.Applicative (empty)
import Control.Monad.State (gets, state)
import Data.Bifunctor (first)
import Data.Maybe (isNothing)
import SimpleParser.Parser (ParserT)
import SimpleParser.Stream (Chunked (chunkLength), Stream (..))

peekToken :: (Stream s, Monad m) => ParserT e s m (Maybe (Token s))
peekToken = gets (fmap fst . streamTake1)

popToken :: (Stream s, Monad m) => ParserT e s m (Maybe (Token s))
popToken = state (\stream -> maybe (Nothing, stream) (first Just) (streamTake1 stream))

peekChunk :: (Stream s, Monad m) => Int -> ParserT e s m (Maybe (Chunk s))
peekChunk n = gets (fmap fst . streamTakeN n)

popChunk :: (Stream s, Monad m) => Int -> ParserT e s m (Maybe (Chunk s))
popChunk n = state (\stream -> maybe (Nothing, stream) (first Just) (streamTakeN n stream))

dropChunk :: (Stream s, Monad m) => Int -> ParserT e s m (Maybe Int)
dropChunk n = state (\stream -> maybe (Nothing, stream) (first Just) (streamDropN n stream))

isEnd :: (Stream s, Monad m) => ParserT e s m Bool
isEnd = isNothing <$> peekToken

matchEnd :: (Stream s, Monad m) => ParserT e s m ()
matchEnd = peekToken >>= maybe (pure ()) (const empty)

anyToken :: (Stream s, Monad m) => ParserT e s m (Token s)
anyToken = popToken >>= maybe empty pure

anyChunk :: (Stream s, Monad m) => Int -> ParserT e s m (Chunk s)
anyChunk n = popChunk n >>= maybe empty pure

satisfyToken :: (Stream s, Monad m) => (Token s -> Bool) -> ParserT e s m (Token s)
satisfyToken p = do
  m <- popToken
  case m of
    Just c | p c -> pure c
    _ -> empty

foldTokensWhile :: (Stream s, Monad m) => (Token s -> x -> (Bool, x)) -> (x -> x) -> x -> ParserT e s m x
foldTokensWhile f g = go where
  go !x = do
    m <- peekToken
    case m of
      Nothing -> pure (g x)
      Just c ->
        let (ok, newX) = f c x
        in if ok
          then popToken *> go newX
          else pure x

takeTokensWhile :: (Stream s, Monad m) => (Token s -> Bool) -> ParserT e s m (Chunk s)
takeTokensWhile = state . streamTakeWhile

dropTokensWhile :: (Stream s, Monad m) => (Token s -> Bool) -> ParserT e s m Int
dropTokensWhile = state . streamDropWhile

matchToken :: (Stream s, Monad m, Eq (Token s)) => Token s -> ParserT e s m (Token s)
matchToken = satisfyToken . (==)

matchChunk :: (Stream s, Monad m, Eq (Chunk s)) => Chunk s -> ParserT e s m (Chunk s)
matchChunk k = popChunk (chunkLength k) >>= maybe empty (\j -> if k == j then pure j else empty)
