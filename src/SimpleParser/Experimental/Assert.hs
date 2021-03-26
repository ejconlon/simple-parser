{-# LANGUAGE UndecidableInstances #-}

module SimpleParser.Experimental.Assert
  ( Predicate (..)
  , PairPredicate (..)
  , TokenPredicate
  , AssertError (..)
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
import Control.Monad.Reader (asks)
import SimpleParser.Chunked (chunkLength)
import SimpleParser.Input (anyChunk, anyToken, dropTokensWhile1, peekToken, popChunk, popToken, takeTokensWhile1)
import SimpleParser.Labels (LabelStack)
import SimpleParser.Parser (ParserT, orParser)
import SimpleParser.Stream (Stream (..))

class Predicate l t g | g -> l t where
  labelPredicate :: g -> l
  runPredicate :: g -> t -> Bool

data PairPredicate l t = PairPredicate
  { ppLabel :: !l
  , ppRun :: !(t -> Bool)
  }

instance Predicate l t (PairPredicate l t) where
  labelPredicate = ppLabel
  runPredicate = ppRun

type TokenPredicate l s g = Predicate l (Token s) g

-- | Failed assertions about common input functions
data AssertError label chunk token =
    AssertMatchEndError !token
  | AssertAnyTokenError
  | AssertAnyChunkError
  | AssertSatisfyTokenError !label !(Maybe token)
  | AssertMatchTokenError !token !(Maybe token)
  | AssertMatchChunkError !chunk !(Maybe chunk)
  | AssertTakeTokensWhile1Error !label !(Maybe token)
  | AssertDropTokensWhile1Error !label !(Maybe token)
  deriving (Eq, Show)

-- | 'AssertError' specialized to 'Stream' types - newtyped to keep GHC happy.
newtype StreamAssertError l s = StreamAssertError
  { unStreamAssertError :: AssertError l (Chunk s) (Token s)
  }

instance (Eq l, Eq (Token s), Eq (Chunk s)) => Eq (StreamAssertError l s) where
  StreamAssertError a == StreamAssertError b = a == b

instance (Show l, Show (Token s), Show (Chunk s)) => Show (StreamAssertError l s) where
  showsPrec d (StreamAssertError e) = showParen (d > 10) (showString "StreamAssertError " . showsPrec (succ d) e)

class MatchStreamAssertError l s e | s e -> l where
  matchStreamAssertError :: e -> Maybe (StreamAssertError l s)

instance MatchStreamAssertError l s (StreamAssertError l s) where
  matchStreamAssertError = Just

-- | Errors that embed 'StreamAssertError'
class MatchStreamAssertError l s e => EmbedStreamAssertError l s e where
  embedStreamAssertError :: StreamAssertError l s -> e

instance EmbedStreamAssertError l s (StreamAssertError l s) where
  embedStreamAssertError = id

-- | Errors that use some monadic state to enrich 'StreamAssertError'
class MatchStreamAssertError l s e => EnrichStreamAssertError l s e where
  enrichStreamAssertError :: LabelStack l -> StreamAssertError l s -> e

instance EnrichStreamAssertError l s (StreamAssertError l s) where
  enrichStreamAssertError = const id

throwStreamAssertError :: (EnrichStreamAssertError l s e, Monad m) => StreamAssertError l s -> ParserT l s e m a
throwStreamAssertError e = asks (`enrichStreamAssertError` e) >>= throwError

throwAssertError :: (EnrichStreamAssertError l s e, Monad m) => AssertError l (Chunk s) (Token s) -> ParserT l s e m a
throwAssertError = throwStreamAssertError . StreamAssertError

catchStreamAssertError :: (MatchStreamAssertError l s e, Monad m) => ParserT l s e m a -> (StreamAssertError l s -> ParserT l s e m a) -> ParserT l s e m a
catchStreamAssertError parser handler = catchError parser (\e -> maybe (throwError e) handler (matchStreamAssertError e))

-- | Contraint for parsers that can throw 'AssertError'
type ParserWithAssert l s e m = (Stream s, EnrichStreamAssertError l s e, Monad m)

-- | 'matchEnd' or throw an 'AssertError'
assertMatchEnd :: ParserWithAssert l s e m => ParserT l s e m ()
assertMatchEnd = peekToken >>= maybe (pure ()) (throwAssertError . AssertMatchEndError)

-- | 'anyToken' or throw an 'AssertError'
assertAnyToken :: ParserWithAssert l s e m => ParserT l s e m (Token s)
assertAnyToken = orParser anyToken (throwAssertError AssertAnyTokenError)

-- | 'anyChunk' or throw an 'AssertError'
assertAnyChunk :: ParserWithAssert l s e m => Int -> ParserT l s e m (Chunk s)
assertAnyChunk n = orParser (anyChunk n) (throwAssertError AssertAnyChunkError)

-- | 'satisfyToken' or throw an 'AssertError'
assertSatisfyToken :: (ParserWithAssert l s e m, Predicate l (Token s) g) => g -> ParserT l s e m (Token s)
assertSatisfyToken pcate = do
  m <- popToken
  case m of
    Just c | runPredicate pcate c -> pure c
    _ -> throwAssertError (AssertSatisfyTokenError (labelPredicate pcate) m)

-- | 'matchToken' or throw an 'AssertError'
assertMatchToken :: (ParserWithAssert l s e m, Eq (Token s)) => Token s -> ParserT l s e m (Token s)
assertMatchToken t = do
  mu <- popToken
  case mu of
    Just u | t == u -> pure u
    _ -> throwAssertError (AssertMatchTokenError t mu)

-- | 'matchChunk' or throw an 'AssertError'
assertMatchChunk :: (ParserWithAssert l s e m, Eq (Chunk s)) => Chunk s -> ParserT l s e m (Chunk s)
assertMatchChunk k = do
  mj <- popChunk (chunkLength k)
  case mj of
    Just j | k == j -> pure j
    _ -> throwAssertError (AssertMatchChunkError k mj)

-- | 'takeTokensWhile1' or throw an 'AssertError'
assertTakeTokensWhile1 :: (ParserWithAssert l s e m, TokenPredicate l s g) => g -> ParserT l s e m (Chunk s)
assertTakeTokensWhile1 pcate = orParser (takeTokensWhile1 (runPredicate pcate)) (peekToken >>= throwAssertError . AssertTakeTokensWhile1Error (labelPredicate pcate))

-- | 'dropTokensWhile1' or throw an 'AssertError'
assertDropTokensWhile1 :: (ParserWithAssert l s e m, TokenPredicate l s g) => g -> ParserT l s e m Int
assertDropTokensWhile1 pcate = orParser (dropTokensWhile1 (runPredicate pcate)) (peekToken >>= throwAssertError . AssertDropTokensWhile1Error (labelPredicate pcate))
