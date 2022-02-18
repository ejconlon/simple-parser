-- | Useful combinators for 'ParserT' and 'Stream'.
-- Classified as SAFE or UNSAFE. SAFE always return a value. UNSAFE throw.
module SimpleParser.Input
  ( withToken
  , withChunk
  , peekToken
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

import Control.Monad.State (gets, state)
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Maybe (isNothing)
import SimpleParser.Chunked (Chunked (..))
import SimpleParser.Parser (ParserT (..), markWithOptStateParser, markWithStateParser)
import SimpleParser.Result (CompoundError (..), ParseError (..), ParseErrorBundle (..), ParseResult (..), RawError (..),
                            StreamError (..))
import SimpleParser.Stack (emptyStack)
import SimpleParser.Stream (Stream (..))

throwStreamError :: Monad m => RawError (Chunk s) (Token s) -> ParserT l s e m a
throwStreamError re = ParserT (\s -> pure (Just (ParseResultError (ParseErrorBundle (pure (ParseError emptyStack s (CompoundErrorStream (StreamError re))))))))

-- | Fetches the next token from the stream and runs the callback.
withToken :: (Stream s, Monad m) => Maybe l -> (Maybe (Token s) -> ParserT l s e m a) -> ParserT l s e m a
withToken ml = markWithOptStateParser ml streamTake1

-- | Fetches the next chunk from the stream and runs the callback.
withChunk :: (Stream s, Monad m) => Maybe l -> Int -> (Maybe (Chunk s) -> ParserT l s e m a) -> ParserT l s e m a
withChunk ml n = markWithOptStateParser ml (streamTakeN n)

-- | Return the next token, if any, but don't consume it. (SAFE)
peekToken :: (Stream s, Monad m) => ParserT l s e m (Maybe (Token s))
peekToken = gets (fmap fst . streamTake1)

-- | Return the next token, if any, and consume it. (SAFE)
popToken :: (Stream s, Monad m) => ParserT l s e m (Maybe (Token s))
popToken = state (\stream -> maybe (Nothing, stream) (first Just) (streamTake1 stream))

-- | Return the next chunk of the given size, if any, but don't consume it.
-- May return a smaller chunk at end of stream, but never returns an empty chunk. (SAFE)
peekChunk :: (Stream s, Monad m) => Int -> ParserT l s e m (Maybe (Chunk s))
peekChunk n = gets (fmap fst . streamTakeN n)

-- | Return the next chunk of the given size, if any, and consume it.
-- May return a smaller chunk at end of stream, but never returns an empty chunk. (SAFE)
popChunk :: (Stream s, Monad m) => Int -> ParserT l s e m (Maybe (Chunk s))
popChunk n = state (\stream -> maybe (Nothing, stream) (first Just) (streamTakeN n stream))

-- | Drop the next chunk of the given size, if any, and consume it.
-- May return a smaller size at end of stream, but never returns size 0. (SAFE)
dropChunk :: (Stream s, Monad m) => Int -> ParserT l s e m (Maybe Int)
dropChunk n = state (\stream -> maybe (Nothing, stream) (first Just) (streamDropN n stream))

-- | Is this the end of the stream? (SAFE)
isEnd :: (Stream s, Monad m) => ParserT l s e m Bool
isEnd = fmap isNothing peekToken

-- | Match the end of the stream or terminate the parser. (UNSAFE)
matchEnd :: (Stream s, Monad m) => ParserT l s e m ()
matchEnd = withToken Nothing (maybe (pure ()) (throwStreamError . RawErrorMatchEnd))

-- | Return the next token or terminate the parser at end of stream. (UNSAFE)
anyToken :: (Stream s, Monad m) => ParserT l s e m (Token s)
anyToken = withToken Nothing (maybe (throwStreamError RawErrorAnyToken) pure)

-- | Return the next chunk of the given size or terminate the parser at end of stream.
-- May return a smaller chunk at end of stream, but never returns an empty chunk. (UNSAFE)
anyChunk :: (Stream s, Monad m) => Int -> ParserT l s e m (Chunk s)
anyChunk n = withChunk Nothing n (maybe (throwStreamError RawErrorAnyChunk) pure)

-- | Match the next token with the given predicate or terminate the parser at predicate false or end of stream. (UNSAFE)
satisfyToken :: (Stream s, Monad m) => Maybe l -> (Token s -> Bool) -> ParserT l s e m (Token s)
satisfyToken ml pcate = withToken ml $ \mu ->
  case mu of
    Just u | pcate u -> pure u
    _ -> throwStreamError (RawErrorSatisfyToken mu)

-- | Folds over a stream of tokens while the boolean value is true.
-- Always succeeds, even at end of stream. Only consumes greediest match. (SAFE)
foldTokensWhile :: (Stream s, Monad m) => (Token s -> x -> (Bool, x)) -> x -> ParserT l s e m x
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
-- Always succeeds, even at end of stream. May return an empty chunk. Only yields greediest match. (SAFE)
takeTokensWhile :: (Stream s, Monad m) => (Token s -> Bool) -> ParserT l s e m (Chunk s)
takeTokensWhile = state . streamTakeWhile

-- | Take tokens into a chunk while they satisfy the given predicate.
-- Only succeeds if 1 or more tokens are taken, so it never returns an empty chunk.
-- Also takes an optional label to describe the predicate. (UNSAFE)
takeTokensWhile1 :: (Stream s, Monad m) => Maybe l -> (Token s -> Bool) -> ParserT l s e m (Chunk s)
takeTokensWhile1 ml pcate = markWithStateParser ml (streamTakeWhile pcate) $ \j ->
  if chunkEmpty j
    then do
      mu <- peekToken
      throwStreamError (RawErrorTakeTokensWhile1 mu)
    else pure j

-- | Drop tokens and return chunk size while they satisfy the given predicate.
-- Always succeeds, even at end of stream. May return empty chunk size 0. Only drops greediest match. (SAFE)
dropTokensWhile :: (Stream s, Monad m) => (Token s -> Bool) -> ParserT l s e m Int
dropTokensWhile = state . streamDropWhile

-- | Drop tokens and return chunk size while they satisfy the given predicate.
-- Only succeeds if 1 or more tokens are dropped.
-- Also takes an optional label to describe the predicate. (UNSAFE)
dropTokensWhile1 :: (Stream s, Monad m) => Maybe l -> (Token s -> Bool) -> ParserT l s e m Int
dropTokensWhile1 ml pcate = markWithStateParser ml (streamDropWhile pcate) $ \s ->
  if s == 0
    then do
      mu <- peekToken
      throwStreamError (RawErrorDropTokensWhile1 mu)
    else pure s

-- | Match token with equality or terminate the parser at inequality or end of stream. (UNSAFE)
matchToken :: (Stream s, Monad m, Eq (Token s)) => Token s -> ParserT l s e m (Token s)
matchToken t = withToken Nothing $ \mu ->
  case mu of
    Just u | t == u -> pure u
    _ -> throwStreamError (RawErrorMatchToken t mu)

-- | Match chunk with equality or terminate the parser at inequality or end of stream. (UNSAFE)
matchChunk :: (Stream s, Monad m, Eq (Chunk s)) => Chunk s -> ParserT l s e m (Chunk s)
matchChunk k = withChunk Nothing (chunkLength k) $ \mj ->
  case mj of
    Just j | k == j -> pure j
    _ -> throwStreamError (RawErrorMatchChunk k mj)
