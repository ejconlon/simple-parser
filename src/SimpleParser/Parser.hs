-- | 'ParserT' is the core monad transformer for parsing.
module SimpleParser.Parser
  ( ParserT (..)
  , Parser
  , runParser
  , filterParser
  , reflectParser
  , unreflectParser
  , andParser
  , andAllParser
  , orParser
  , orAllParser
  , suppressParser
  , isolateParser
  , defaultParser
  , defaultErrorParser
  , optionalParser
  , silenceParser
  , greedyStarParser
  , greedyStarParser_
  , greedyPlusParser
  , greedyPlusParser_
  , lookAheadParser
  , mapErrorStateParser
  , traverseErrorStateParser
  , voidErrorParser
  ) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad (MonadPlus (..), ap, (>=>))
import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Foldable (toList)
import Data.Void (Void)
import ListT (ListT (..))
import qualified ListT
import SimpleParser.Result (ParseResult (..), ParseValue (..))

-- | A 'ParserT' is a state/error/list transformer useful for parsing.
-- All MTL instances are for this transformer only. If, for example, your effect
-- has its own 'MonadState' instance, you'll have to use 'lift get' instead of 'get'.
newtype ParserT e s m a = ParserT { runParserT :: s -> ListT m (ParseResult e s a) }
  deriving (Functor)

-- | Use 'Parser' if you have no need for other monadic effects.
type Parser e s a = ParserT e s Identity a

instance Monad m => Applicative (ParserT e s m) where
  pure a = ParserT (pure . ParseResult (ParseSuccess a))
  (<*>) = ap

instance Monad m => Monad (ParserT e s m) where
  return = pure
  parser >>= f = ParserT (runParserT parser >=> go) where
    go (ParseResult v t) =
      case v of
        ParseError e -> pure (ParseResult (ParseError e) t)
        ParseSuccess a -> runParserT (f a) t

instance Monad m => Alternative (ParserT e s m) where
  empty = ParserT (const empty)
  (<|>) = andParser

instance Monad m => MonadPlus (ParserT e s m) where
  mzero = empty
  mplus = (<|>)

instance Monad m => MonadError e (ParserT e s m) where
  throwError e = ParserT (pure . ParseResult (ParseError e))
  catchError parser handler = ParserT (runParserT parser >=> go) where
    go (ParseResult v t) =
      case v of
        ParseError e -> runParserT (handler e) t
        ParseSuccess a -> pure (ParseResult (ParseSuccess a) t)

instance Monad m => MonadState s (ParserT e s m) where
  get = ParserT (\s -> pure (ParseResult (ParseSuccess s) s))
  put t = ParserT (const (pure (ParseResult (ParseSuccess ()) t)))
  state f = ParserT (\s -> let (a, t) = f s in pure (ParseResult (ParseSuccess a) t))

instance MonadTrans (ParserT e s) where
  lift ma = ParserT (\s -> lift (fmap (\a -> ParseResult (ParseSuccess a) s) ma))

instance MFunctor (ParserT e s) where
  hoist trans (ParserT f) = ParserT (hoist trans . f)

-- | Runs a non-effectful parser from an inital state and collects all results.
runParser :: Parser e s a -> s -> [ParseResult e s a]
runParser m s = runIdentity (ListT.toList (runParserT m s))

-- | Filters parse results
filterParser :: Monad m => (a -> Bool) -> ParserT e s m a -> ParserT e s m a
filterParser f parser = ParserT (ListT . go . runParserT parser) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (r@(ParseResult v _), rest) ->
        case v of
          ParseSuccess a | not (f a) -> go rest
          _ -> pure (Just (r, ListT (go rest)))

-- | A kind of "catch" that returns all results, success and failure.
reflectParser :: Monad m => ParserT e s m a -> ParserT e s m (ParseValue e a)
reflectParser parser = ParserT (ListT . go . runParserT parser) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (ParseResult v t, rest) ->
        pure (Just (ParseResult (ParseSuccess v) t, ListT (go rest)))

-- | A kind of "throw" that is the inverse of 'reflectParser'.
unreflectParser :: Monad m => ParserT e s m (ParseValue e a) -> ParserT e s m a
unreflectParser parser = ParserT (ListT . go . runParserT parser) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (ParseResult v t, rest) ->
        let nextHead = case v of
              ParseError e -> ParseError e
              ParseSuccess w -> w
        in pure (Just (ParseResult nextHead t, ListT (go rest)))

-- | Combines the results of two parsers. Equivalent to '<|>'
andParser :: Monad m => ParserT e s m a -> ParserT e s m a -> ParserT e s m a
andParser first second = ParserT (\s -> ListT (go s (runParserT first s))) where
  go s listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> ListT.uncons (runParserT second s)
      Just (r, nextListt) -> pure (Just (r, ListT (go s nextListt)))

-- | Combines the results of all parsers. Equvalent to 'asum'.
andAllParser :: (Foldable f, Monad m) => f (ParserT e s m a) -> ParserT e s m a
andAllParser = start . toList where
  start ps =
    case ps of
      [] -> empty
      q:qs -> ParserT (\s -> ListT (run s qs (runParserT q s)))
  run s qs listt = do
    m <- ListT.uncons listt
    case m of
      Nothing ->
        case qs of
          [] -> pure Nothing
          r:rs -> run s rs (runParserT r s)
      Just (a, nextListt) -> pure (Just (a, ListT (run s qs nextListt)))

-- | Yields results from the FIRST parser of the two that returns ANY results (success or failure).
orParser :: Monad m => ParserT e s m a -> ParserT e s m a -> ParserT e s m a
orParser first second = ParserT (\s -> ListT (go s (runParserT first s))) where
  go s listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> ListT.uncons (runParserT second s)
      Just _ -> pure m

-- | Yields results from the FIRST parser of the list that returns ANY results (success or failure).
orAllParser :: (Monad m, Foldable f) => f (ParserT e s m a) -> ParserT e s m a
orAllParser = start . toList where
  start ps =
    case ps of
      [] -> empty
      q:qs -> ParserT (\s -> ListT (run s qs (runParserT q s)))
  run s qs listt = do
    m <- ListT.uncons listt
    case m of
      Nothing ->
        case qs of
          [] -> pure Nothing
          r:rs -> run s rs (runParserT r s)
      Just _ -> pure m

gatherParser :: Monad m => Bool -> ParserT e s m a -> ParserT e s m a
gatherParser single parser = ParserT (ListT . go [] . runParserT parser) where
  go !acc listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> returnErr (reverse acc)
      Just (r@(ParseResult v _), rest) ->
        case v of
          ParseError _ -> go (r:acc) rest
          ParseSuccess _ ->
            let t = if single then empty else ListT (filterOk rest)
            in pure (Just (r, t))

  returnErr racc =
    case racc of
      [] -> pure Nothing
      r:rs -> pure (Just (r, ListT (returnErr rs)))

  filterOk listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (r@(ParseResult v _), rest) ->
        let nextListt = filterOk rest
        in case v of
          ParseError _ -> nextListt
          ParseSuccess _ -> pure (Just (r, ListT nextListt))

-- | If the parse results in ANY successes, keep only those. Otherwise return all failures.
-- This may block indefinitely as it awaits either the end of the parser or its first success.
-- See 'isolateParser' if you want only one success.
suppressParser :: Monad m => ParserT e s m a -> ParserT e s m a
suppressParser = gatherParser False

-- | If the parse results in ANY successes, keep only THE FIRST. Otherwise return all failures.
-- This may block indefinitely as it awaits either the end of the parser or its first success.
-- See 'suppressParser' if you want all successes.
isolateParser :: Monad m => ParserT e s m a -> ParserT e s m a
isolateParser = gatherParser True

defaultValueParser :: Monad m => ParseValue e a -> ParserT e s m a -> ParserT e s m a
defaultValueParser val parser = ParserT (\s -> ListT (go s (runParserT parser s))) where
  go s listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure (Just (ParseResult val s, empty))
      Just _ -> pure m

-- | If the parser yields no results (success or failure), yield a given value.
defaultParser :: Monad m => a -> ParserT e s m a -> ParserT e s m a
defaultParser = defaultValueParser . ParseSuccess

-- | If the parser yields no results (success or failure), throw the given error.
defaultErrorParser :: Monad m => e -> ParserT e s m a -> ParserT e s m a
defaultErrorParser = defaultValueParser . ParseError

-- | A parser that yields 'Nothing' if there are no results (success or failure),
-- otherwise wrapping successes in 'Just'.
optionalParser :: Monad m => ParserT e s m a -> ParserT e s m (Maybe a)
optionalParser parser = defaultParser Nothing (fmap Just parser)

-- | Removes all failures from the parse results.
-- Equivalent to 'catchError (const empty)'.
silenceParser :: Monad m => ParserT e s m a -> ParserT e s m a
silenceParser parser = ParserT (ListT . go . runParserT parser) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (r@(ParseResult v _), rest) ->
        let nextListt = go rest
        in case v of
          ParseError _ -> nextListt
          ParseSuccess _ -> pure (Just (r, ListT nextListt))

-- | Yields the LONGEST string of 0 or more successes of the given parser (and passes through failures).
greedyStarParser :: Monad m => ParserT e s m a -> ParserT e s m [a]
greedyStarParser parser = go [] where
  opt = optionalParser parser
  go !acc = do
    res <- opt
    case res of
      Nothing -> pure (reverse acc)
      Just a -> go (a:acc)

-- | Same as 'greedyStarParser' but discards the result.
greedyStarParser_ :: Monad m => ParserT e s m a -> ParserT e s m ()
greedyStarParser_ parser = go where
  opt = optionalParser parser
  go = do
    res <- opt
    case res of
      Nothing -> pure ()
      Just _ -> go

-- | Yields the LONGEST string of 1 or more successes of the given parser (and passes through failures).
greedyPlusParser :: Monad m => ParserT e s m a -> ParserT e s m [a]
greedyPlusParser parser = liftA2 (:) parser (greedyStarParser parser)

-- | Same as 'greedyPlusParser' but discards the result.
greedyPlusParser_ :: Monad m => ParserT e s m a -> ParserT e s m ()
greedyPlusParser_ parser = parser *> greedyStarParser_ parser

-- | Yield the results of the given parser, but rewind back to the starting state.
-- Note that these results may contain errors, so you may want to stifle them with 'silenceParser', for example.
lookAheadParser :: Monad m => ParserT e s m a -> ParserT e s m a
lookAheadParser parser = do
  s <- get
  flip catchError (\e -> put s *> throwError e) $ do
    v <- parser
    put s
    pure v

-- | Traverse the error and thrown state to change the error type of the parser.
traverseErrorStateParser :: Monad m => (e -> s -> m d) -> ParserT e s m a -> ParserT d s m a
traverseErrorStateParser f p = ParserT (ListT . go . runParserT p) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (ParseResult v t, nextListt) -> do
        w <- case v of
          ParseError e -> fmap ParseError (f e t)
          ParseSuccess a -> pure (ParseSuccess a)
        pure (Just (ParseResult w t, ListT (go nextListt)))

-- | Map over the error and thrown state to change the error type of the parser.
mapErrorStateParser :: Monad m => (e -> s -> d) -> ParserT e s m a -> ParserT d s m a
mapErrorStateParser f = traverseErrorStateParser (\e s -> pure (f e s))

-- | Drop all errors from the parse results. Unlike 'silenceParser', this
-- changes the error type of the parser so that no further errors can be thrown.
voidErrorParser :: Monad m => ParserT e s m a -> ParserT Void s m a
voidErrorParser p = ParserT (runParserT p >=> go) where
  go (ParseResult v t) =
    case v of
      ParseError _ -> empty
      ParseSuccess a -> pure (ParseResult (ParseSuccess a) t)
