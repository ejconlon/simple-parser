-- | Useful combinators for 'ParserT' and 'Stream'.
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
  , takeTokensWhile1
  , dropTokensWhile
  , dropTokensWhile1
  , matchToken
  , matchChunk
  ) where

import Control.Applicative (empty)
import Control.Monad.State (gets, state)
import Data.Bifunctor (first)
import Data.Maybe (isNothing)
import SimpleParser.Parser (ParserT)
import SimpleParser.Stream (Chunked (..), Stream (..))

-- | Return the next token, if any, but don't consume it.
peekToken :: (Stream s, Monad m) => ParserT e s m (Maybe (Token s))
peekToken = gets (fmap fst . streamTake1)

-- | Return the next token, if any, and consume it.
popToken :: (Stream s, Monad m) => ParserT e s m (Maybe (Token s))
popToken = state (\stream -> maybe (Nothing, stream) (first Just) (streamTake1 stream))

-- | Return the next chunk of the given size, if any, but don't consume it.
-- May return a smaller chunk at end of stream, but never returns an empty chunk.
peekChunk :: (Stream s, Monad m) => Int -> ParserT e s m (Maybe (Chunk s))
peekChunk n = gets (fmap fst . streamTakeN n)

-- | Return the next chunk of the given size, if any, and consume it.
-- May return a smaller chunk at end of stream, but never returns an empty chunk.
popChunk :: (Stream s, Monad m) => Int -> ParserT e s m (Maybe (Chunk s))
popChunk n = state (\stream -> maybe (Nothing, stream) (first Just) (streamTakeN n stream))

-- | Drop the next chunk of the given size, if any, and consume it.
-- May return a smaller size at end of stream, but never returns size 0.
dropChunk :: (Stream s, Monad m) => Int -> ParserT e s m (Maybe Int)
dropChunk n = state (\stream -> maybe (Nothing, stream) (first Just) (streamDropN n stream))

-- | Is this the end of the stream?
isEnd :: (Stream s, Monad m) => ParserT e s m Bool
isEnd = isNothing <$> peekToken

-- | Match the end of the stream or terminate the parser.
matchEnd :: (Stream s, Monad m) => ParserT e s m ()
matchEnd = peekToken >>= maybe (pure ()) (const empty)

-- | Return the next token or terminate the parser at end of stream.
anyToken :: (Stream s, Monad m) => ParserT e s m (Token s)
anyToken = popToken >>= maybe empty pure

-- | Return the next chunk of the given size or terminate the parser at end of stream.
-- May return a smaller chunk at end of stream, but never returns an empty chunk.
anyChunk :: (Stream s, Monad m) => Int -> ParserT e s m (Chunk s)
anyChunk n = popChunk n >>= maybe empty pure

-- | Match the next token with the given predicate or terminate the parser at predicate false or end of stream.
satisfyToken :: (Stream s, Monad m) => (Token s -> Bool) -> ParserT e s m (Token s)
satisfyToken p = do
  m <- popToken
  case m of
    Just c | p c -> pure c
    _ -> empty

-- | Folds over a stream of tokens while the boolean value is true.
-- Always succeeds, even at end of stream. Only consumes greediest match.
foldTokensWhile :: (Stream s, Monad m) => (Token s -> x -> (Bool, x)) -> x -> ParserT e s m x
foldTokensWhile processNext = go where
  go !x = do
    m <- peekToken
    case m of
      Nothing -> pure x
      Just c ->
        let (ok, newX) = processNext c x
        in if ok
          then popToken *> go newX
          else pure x

-- | Take tokens into a chunk while they satisfy the given predicate.
-- Always succeeds, even at end of stream. May return an empty chunk. Only yields greediest match.
takeTokensWhile :: (Stream s, Monad m) => (Token s -> Bool) -> ParserT e s m (Chunk s)
takeTokensWhile = state . streamTakeWhile

-- | Take tokens into a chunk while they satisfy the given predicate.
-- Only succeeds if 1 or more tokens are taken, so it never returns an empty chunk.
takeTokensWhile1 :: (Stream s, Monad m) => (Token s -> Bool) -> ParserT e s m (Chunk s)
takeTokensWhile1 pcate = takeTokensWhile pcate >>= \c -> if chunkEmpty c then empty else pure c

-- | Drop tokens and return chunk size while they satisfy the given predicate.
-- Always succeeds, even at end of stream. May return empty chunk size 0. Only drops greediest match.
dropTokensWhile :: (Stream s, Monad m) => (Token s -> Bool) -> ParserT e s m Int
dropTokensWhile = state . streamDropWhile

-- | Drop tokens and return chunk size while they satisfy the given predicate.
-- Only succeeds if 1 or more tokens are dropped.
dropTokensWhile1 :: (Stream s, Monad m) => (Token s -> Bool) -> ParserT e s m Int
dropTokensWhile1 pcate = dropTokensWhile pcate >>= \s -> if s == 0 then empty else pure s

-- | Match token with equality or terminate the parser at inequality or end of stream.
matchToken :: (Stream s, Monad m, Eq (Token s)) => Token s -> ParserT e s m (Token s)
matchToken = satisfyToken . (==)

-- | Match chunk with equality or terminate the parser at inequality or end of stream.
matchChunk :: (Stream s, Monad m, Eq (Chunk s)) => Chunk s -> ParserT e s m (Chunk s)
matchChunk k = popChunk (chunkLength k) >>= maybe empty (\j -> if k == j then pure j else empty)
