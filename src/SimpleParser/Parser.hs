-- | 'ParserT' is the core monad transformer for parsing.
module SimpleParser.Parser
  ( ParserT (..)
  , Parser
  , runParser
  , filterParser
  , valueParser
  , reflectParser
  , unreflectParser
  , andParser
  , andAllParser
  , orParser
  , orAllParser
  , suppressParser
  , isolateParser
  , defaultValueParser
  , defaultSuccessParser
  , defaultErrorParser
  , optionalParser
  , silenceParser
  , greedyStarParser
  , greedyStarParser_
  , greedyPlusParser
  , greedyPlusParser_
  , lookAheadParser
  , mapErrorParser
  , traverseErrorParser
  , voidErrorParser
  ) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad (MonadPlus (..), ap)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Foldable (toList)
import Data.Void (Void)
import ListT (ListT (..))
import qualified ListT
import SimpleParser.Chunked (Chunked (..))
import SimpleParser.Result (ParseResult (..), ParseValue (..))

-- | A 'ParserT' is a state/error/list transformer useful for parsing.
-- All MTL instances are for this transformer only. If, for example, your effect
-- has its own 'MonadState' instance, you'll have to use 'lift get' instead of 'get'.
newtype ParserT r s e m a = ParserT { runParserT :: r -> s -> ListT m (ParseResult s e a) }
  deriving (Functor)

-- | Use 'Parser' if you have no need for other monadic effects.
type Parser r s e a = ParserT r s e Identity a

instance Monad m => Applicative (ParserT r s e m) where
  pure a = ParserT (\_ s -> pure (ParseResult s (ParseSuccess a)))
  (<*>) = ap

instance Monad m => Monad (ParserT r s e m) where
  return = pure
  parser >>= f = ParserT (\r s -> runParserT parser r s >>= go r) where
    go r (ParseResult t v) =
      case v of
        ParseError e -> pure (ParseResult t (ParseError e))
        ParseSuccess a -> runParserT (f a) r t

instance Monad m => Alternative (ParserT r s e m) where
  empty = ParserT (\_ _ -> empty)
  (<|>) = andParser

instance Monad m => MonadPlus (ParserT r s e m) where
  mzero = empty
  mplus = (<|>)

instance Monad m => MonadReader r (ParserT r s e m) where
  ask = ParserT (\r s -> pure (ParseResult s (ParseSuccess r)))
  reader f = ParserT (\r s -> pure (ParseResult s (ParseSuccess (f r))))
  local f p = ParserT (runParserT p . f)

instance Monad m => MonadState s (ParserT r s e m) where
  get = ParserT (\_ s -> pure (ParseResult s (ParseSuccess s)))
  put t = ParserT (\_ _ -> pure (ParseResult t (ParseSuccess ())))
  state f = ParserT (\_ s -> let (!a, !t) = f s in pure (ParseResult t (ParseSuccess a)))

instance Monad m => MonadError e (ParserT r s e m) where
  throwError e = ParserT (\_ s -> pure (ParseResult s (ParseError e)))
  catchError parser handler = ParserT (\r s -> runParserT parser r s >>= go r) where
    go r (ParseResult t v) =
      case v of
        ParseError e -> runParserT (handler e) r t
        ParseSuccess a -> pure (ParseResult t (ParseSuccess a))

instance MonadTrans (ParserT r s e) where
  lift ma = ParserT (\_ s -> lift (fmap (ParseResult s . ParseSuccess) ma))

instance MFunctor (ParserT r s e) where
  hoist trans (ParserT f) = ParserT (\r s -> hoist trans (f r s))

-- | Runs a non-effectful parser from an inital state and collects all results.
runParser :: Parser r s e a -> r -> s -> [ParseResult s e a]
runParser m r s = runIdentity (ListT.toList (runParserT m r s))

-- | Filters parse results
filterParser :: Monad m => (a -> Bool) -> ParserT r s e m a -> ParserT r s e m a
filterParser f parser = ParserT (\r s -> ListT (go (runParserT parser r s))) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (r@(ParseResult _ v), rest) ->
        case v of
          ParseSuccess a | not (f a) -> go rest
          _ -> pure (Just (r, ListT (go rest)))

-- | A kind of 'pure' or 'throwError' for 'ParseValue.
valueParser :: Monad m => ParseValue e a -> ParserT r s e m a
valueParser v = ParserT (\_ s -> pure (ParseResult s v))

-- | A kind of 'catchError' that returns all results, success and failure.
reflectParser :: Monad m => ParserT r s e m a -> ParserT r s e m (ParseValue e a)
reflectParser parser = ParserT (\r s -> ListT (go (runParserT parser r s))) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (ParseResult t v, rest) ->
        pure (Just (ParseResult t (ParseSuccess v), ListT (go rest)))

-- | A kind of 'throwError' that is the inverse of 'reflectParser'.
unreflectParser :: Monad m => ParserT r s e m (ParseValue e a) -> ParserT r s e m a
unreflectParser parser = ParserT (\r s -> ListT (go (runParserT parser r s))) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (ParseResult t v, rest) ->
        let a = case v of
              ParseError e -> ParseError e
              ParseSuccess w -> w
        in pure (Just (ParseResult t a, ListT (go rest)))

-- -- | Combines the results of two parsers. Equivalent to '<|>'
andParser :: Monad m => ParserT r s e m a -> ParserT r s e m a -> ParserT r s e m a
andParser first second = ParserT (\r s -> ListT (go r s (runParserT first r s))) where
  go r s listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> ListT.uncons (runParserT second r s)
      Just (a, nextListt) -> pure (Just (a, ListT (go r s nextListt)))

-- | Combines the results of all parsers. Equvalent to 'asum'.
andAllParser :: (Foldable f, Monad m) => f (ParserT r s e m a) -> ParserT r s e m a
andAllParser = start . toList where
  start ps =
    case ps of
      [] -> empty
      q:qs -> ParserT (\r s -> ListT (run r s qs (runParserT q r s)))
  run r s qs listt = do
    m <- ListT.uncons listt
    case m of
      Nothing ->
        case qs of
          [] -> pure Nothing
          x:xs -> run r s xs (runParserT x r s)
      Just (a, nextListt) -> pure (Just (a, ListT (run r s qs nextListt)))

-- | Yields results from the FIRST parser of the two that returns ANY results (success or failure).
orParser :: Monad m => ParserT r s e m a -> ParserT r s e m a -> ParserT r s e m a
orParser first second = ParserT (\r s -> ListT (go r s (runParserT first r s))) where
  go r s listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> ListT.uncons (runParserT second r s)
      Just _ -> pure m

-- | Yields results from the FIRST parser of the list that returns ANY results (success or failure).
orAllParser :: (Monad m, Foldable f) => f (ParserT r s e m a) -> ParserT r s e m a
orAllParser = start . toList where
  start ps =
    case ps of
      [] -> empty
      q:qs -> ParserT (\r s -> ListT (run r s qs (runParserT q r s)))
  run r s qs listt = do
    m <- ListT.uncons listt
    case m of
      Nothing ->
        case qs of
          [] -> pure Nothing
          x:xs -> run r s xs (runParserT x r s)
      Just _ -> pure m

gatherParser :: Monad m => Bool -> ParserT r s e m a -> ParserT r s e m a
gatherParser single parser = ParserT (\r s -> ListT (go [] (runParserT parser r s))) where
  go !racc listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> returnErr (reverse racc)
      Just (res@(ParseResult _ v), rest) ->
        case v of
          ParseError _ -> go (res:racc) rest
          ParseSuccess _ ->
            let nextListt = if single then empty else ListT (filterOk rest)
            in pure (Just (res, nextListt))

  returnErr racc =
    case racc of
      [] -> pure Nothing
      res:rs -> pure (Just (res, ListT (returnErr rs)))

  filterOk listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (res@(ParseResult _ v), rest) ->
        let nextListt = filterOk rest
        in case v of
          ParseError _ -> nextListt
          ParseSuccess _ -> pure (Just (res, ListT nextListt))

-- | If the parse results in ANY successes, keep only those. Otherwise return all failures.
-- This may block indefinitely as it awaits either the end of the parser or its first success.
-- See 'isolateParser' if you want only one success.
suppressParser :: Monad m => ParserT r s e m a -> ParserT r s e m a
suppressParser = gatherParser False

-- | If the parse results in ANY successes, keep only THE FIRST. Otherwise return all failures.
-- This may block indefinitely as it awaits either the end of the parser or its first success.
-- See 'suppressParser' if you want all successes.
isolateParser :: Monad m => ParserT r s e m a -> ParserT r s e m a
isolateParser = gatherParser True

defaultValueParser :: Monad m => ParseValue e a -> ParserT r s e m a -> ParserT r s e m a
defaultValueParser val parser = ParserT (\r s -> ListT (go s (runParserT parser r s))) where
  go s listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure (Just (ParseResult s val, empty))
      Just _ -> pure m

-- | If the parser yields no results (success or failure), yield a given value.
defaultSuccessParser :: Monad m => a -> ParserT r s e m a -> ParserT r s e m a
defaultSuccessParser = defaultValueParser . ParseSuccess

-- | If the parser yields no results (success or failure), throw the given error.
defaultErrorParser :: Monad m => e -> ParserT r s e m a -> ParserT r s e m a
defaultErrorParser = defaultValueParser . ParseError

-- | A parser that yields 'Nothing' if there are no results (success or failure),
-- otherwise wrapping successes in 'Just'.
optionalParser :: Monad m => ParserT r s e m a -> ParserT r s e m (Maybe a)
optionalParser parser = defaultSuccessParser Nothing (fmap Just parser)

-- | Removes all failures from the parse results.
-- Equivalent to 'catchError (const empty)'.
silenceParser :: Monad m => ParserT r s e m a -> ParserT r s e m a
silenceParser parser = ParserT (\r s -> ListT (go (runParserT parser r s))) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (res@(ParseResult _ v), rest) ->
        let nextListt = go rest
        in case v of
          ParseError _ -> nextListt
          ParseSuccess _ -> pure (Just (res, ListT nextListt))

-- | Yields the LONGEST string of 0 or more successes of the given parser (and passes through failures).
greedyStarParser :: (Chunked seq elem, Monad m) => ParserT r s e m elem -> ParserT r s e m seq
greedyStarParser parser = go [] where
  opt = optionalParser parser
  go !acc = do
    res <- opt
    case res of
      Nothing -> pure (revTokensToChunk acc)
      Just a -> go (consChunk a acc)

-- | Same as 'greedyStarParser' but discards the result.
greedyStarParser_ :: Monad m => ParserT r s e m a -> ParserT r s e m ()
greedyStarParser_ parser = go where
  opt = optionalParser parser
  go = do
    res <- opt
    case res of
      Nothing -> pure ()
      Just _ -> go

-- | Yields the LONGEST string of 1 or more successes of the given parser (and passes through failures).
greedyPlusParser :: (Chunked seq elem, Monad m) => ParserT r s e m elem -> ParserT r s e m seq
greedyPlusParser parser = liftA2 consChunk parser (greedyStarParser parser)

-- | Same as 'greedyPlusParser' but discards the result.
greedyPlusParser_ :: Monad m => ParserT r s e m a -> ParserT r s e m ()
greedyPlusParser_ parser = parser *> greedyStarParser_ parser

-- | Yield the results of the given parser, but rewind back to the starting state.
-- Note that these results may contain errors, so you may want to stifle them with 'silenceParser', for example.
lookAheadParser :: Monad m => ParserT r s e m a -> ParserT r s e m a
lookAheadParser parser = do
  s <- get
  flip catchError (\e -> put s *> throwError e) $ do
    v <- parser
    put s
    pure v

-- | Traverse the error and thrown state to change the error type of the parser.
traverseErrorParser :: Monad m => (s -> e -> m d) -> ParserT r s e m a -> ParserT r s d m a
traverseErrorParser f p = ParserT (\r s -> ListT (go (runParserT p r s))) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (ParseResult t v, nextListt) -> do
        w <- case v of
          ParseError e -> fmap ParseError (f t e)
          ParseSuccess a -> pure (ParseSuccess a)
        pure (Just (ParseResult t w, ListT (go nextListt)))

-- | Map over the error and thrown state to change the error type of the parser.
mapErrorParser :: Monad m => (s -> e -> d) -> ParserT r s e m a -> ParserT r s d m a
mapErrorParser f = traverseErrorParser (\e s -> pure (f e s))

-- | Drop all errors from the parse results. Unlike 'silenceParser', this
-- changes the error type of the parser so that no further errors can be thrown.
voidErrorParser :: Monad m => ParserT r s e m a -> ParserT r s Void m a
voidErrorParser p = ParserT (\r s -> runParserT p r s >>= go) where
  go (ParseResult t v) =
    case v of
      ParseError _ -> empty
      ParseSuccess a -> pure (ParseResult t (ParseSuccess a))
