-- | 'ParserT' is the core monad transformer for parsing.
module SimpleParser.Parser
  ( ParserT (..)
  , Parser
  , runParser
  , catchJustParser
  , filterParser
  , reflectParser
  , andParser
  , andAllParser
  , orParser
  , orAllParser
  , suppressParser
  , isolateParser
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
import SimpleParser.Labels (LabelStack (..), LabelledError (..))
import SimpleParser.Result (CompoundError (..), ParseResult (..), ParseValue (..))

-- | A 'ParserT' is a state/error/list transformer useful for parsing.
-- All MTL instances are for this transformer only. If, for example, your effect
-- has its own 'MonadState' instance, you'll have to use 'lift get' instead of 'get'.
newtype ParserT l s e m a = ParserT { runParserT :: LabelStack l -> s -> ListT m (ParseResult l s e a) }
  deriving (Functor)

-- | Use 'Parser' if you have no need for other monadic effects.
type Parser l s e a = ParserT l s e Identity a

instance Monad m => Applicative (ParserT l s e m) where
  pure a = ParserT (\_ s -> pure (ParseResult s (ParseValueSuccess a)))
  (<*>) = ap

instance Monad m => Monad (ParserT l s e m) where
  return = pure
  parser >>= f = ParserT (\ls s -> runParserT parser ls s >>= go ls) where
    go ls (ParseResult t v) =
      case v of
        ParseValueError e -> pure (ParseResult t (ParseValueError e))
        ParseValueSuccess a -> runParserT (f a) ls t

instance Monad m => Alternative (ParserT l s e m) where
  empty = ParserT (\_ _ -> empty)
  (<|>) = andParser

instance Monad m => MonadPlus (ParserT l s e m) where
  mzero = empty
  mplus = (<|>)

instance Monad m => MonadReader (LabelStack l) (ParserT l s e m) where
  ask = ParserT (\ls s -> pure (ParseResult s (ParseValueSuccess ls)))
  reader f = ParserT (\ls s -> pure (ParseResult s (ParseValueSuccess (f ls))))
  local f p = ParserT (runParserT p . f)

instance Monad m => MonadState s (ParserT l s e m) where
  get = ParserT (\_ s -> pure (ParseResult s (ParseValueSuccess s)))
  put t = ParserT (\_ _ -> pure (ParseResult t (ParseValueSuccess ())))
  state f = ParserT (\_ s -> let (!a, !t) = f s in pure (ParseResult t (ParseValueSuccess a)))

instance Monad m => MonadError e (ParserT l s e m) where
  throwError e = ParserT (\ls s -> pure (ParseResult s (ParseValueError (LabelledError ls (CompoundErrorCustom e)))))
  catchError parser handler = ParserT (\ls s -> runParserT parser ls s >>= go ls) where
    go ls res@(ParseResult t v) =
      case v of
        ParseValueError (LabelledError _ ce) ->
          case ce of
            CompoundErrorCustom e -> runParserT (handler e) ls t
            _ -> pure res
        ParseValueSuccess a -> pure (ParseResult t (ParseValueSuccess a))

instance MonadTrans (ParserT l s e) where
  lift ma = ParserT (\_ s -> lift (fmap (ParseResult s . ParseValueSuccess) ma))

instance MFunctor (ParserT l s e) where
  hoist trans (ParserT f) = ParserT (\ls s -> hoist trans (f ls s))

-- | Runs a non-effectful parser from an inital state and collects all results.
runParser :: Parser l s e a -> LabelStack l -> s -> [ParseResult l s e a]
runParser parser ls s = runIdentity (ListT.toList (runParserT parser ls s))

-- | Catch only a subset of errors. This preserves label information vs rethrowing.
catchJustParser :: Monad m => (e -> Maybe a) -> ParserT l s e m a -> ParserT l s e m a
catchJustParser handler parser = ParserT (\ls s -> ListT (go (runParserT parser ls s))) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (res@(ParseResult s v), rest) ->
        let nextListt = ListT (go rest)
        in case v of
          ParseValueError (LabelledError _ (CompoundErrorCustom e) )->
            case handler e of
              Nothing -> pure (Just (res, nextListt))
              Just a -> pure (Just (ParseResult s (ParseValueSuccess a), nextListt))
          _ -> pure (Just (res, nextListt))

-- | Filters parse results
filterParser :: Monad m => (a -> Bool) -> ParserT l s e m a -> ParserT l s e m a
filterParser f parser = ParserT (\ls s -> ListT (go (runParserT parser ls s))) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (r@(ParseResult _ v), rest) ->
        case v of
          ParseValueSuccess a | not (f a) -> go rest
          _ -> pure (Just (r, ListT (go rest)))

-- | A kind of 'catchError' that returns all results, success and failure.
reflectParser :: Monad m => ParserT l s e m a -> ParserT l s e m (ParseValue l s e a)
reflectParser parser = ParserT (\ls s -> ListT (go (runParserT parser ls s))) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (ParseResult t v, rest) ->
        pure (Just (ParseResult t (ParseValueSuccess v), ListT (go rest)))

-- -- | Combines the results of two parsers. Equivalent to '<|>'
andParser :: Monad m => ParserT l s e m a -> ParserT l s e m a -> ParserT l s e m a
andParser first second = ParserT (\ls s -> ListT (go ls s (runParserT first ls s))) where
  go r s listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> ListT.uncons (runParserT second r s)
      Just (a, nextListt) -> pure (Just (a, ListT (go r s nextListt)))

-- | Combines the results of all parsers. Equvalent to 'asum'.
andAllParser :: (Foldable f, Monad m) => f (ParserT l s e m a) -> ParserT l s e m a
andAllParser = start . toList where
  start ps =
    case ps of
      [] -> empty
      q:qs -> ParserT (\ls s -> ListT (run ls s qs (runParserT q ls s)))
  run ls s qs listt = do
    m <- ListT.uncons listt
    case m of
      Nothing ->
        case qs of
          [] -> pure Nothing
          x:xs -> run ls s xs (runParserT x ls s)
      Just (a, nextListt) -> pure (Just (a, ListT (run ls s qs nextListt)))

-- | Yields results from the FIRST parser of the two that returns ANY results (success or failure).
orParser :: Monad m => ParserT l s e m a -> ParserT l s e m a -> ParserT l s e m a
orParser first second = ParserT (\ls s -> ListT (go ls s (runParserT first ls s))) where
  go r s listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> ListT.uncons (runParserT second r s)
      Just _ -> pure m

-- | Yields results from the FIRST parser of the list that returns ANY results (success or failure).
orAllParser :: (Monad m, Foldable f) => f (ParserT l s e m a) -> ParserT l s e m a
orAllParser = start . toList where
  start ps =
    case ps of
      [] -> empty
      q:qs -> ParserT (\ls s -> ListT (run ls s qs (runParserT q ls s)))
  run r s qs listt = do
    m <- ListT.uncons listt
    case m of
      Nothing ->
        case qs of
          [] -> pure Nothing
          x:xs -> run r s xs (runParserT x r s)
      Just _ -> pure m

gatherParser :: Monad m => Bool -> ParserT l s e m a -> ParserT l s e m a
gatherParser single parser = ParserT (\ls s -> ListT (go [] (runParserT parser ls s))) where
  go !racc listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> returnErr (reverse racc)
      Just (res@(ParseResult _ v), rest) ->
        case v of
          ParseValueError _ -> go (res:racc) rest
          ParseValueSuccess _ ->
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
          ParseValueError _ -> nextListt
          ParseValueSuccess _ -> pure (Just (res, ListT nextListt))

-- | If the parse results in ANY successes, keep only those. Otherwise return all failures.
-- This may block indefinitely as it awaits either the end of the parser or its first success.
-- See 'isolateParser' if you want only one success.
suppressParser :: Monad m => ParserT l s e m a -> ParserT l s e m a
suppressParser = gatherParser False

-- | If the parse results in ANY successes, keep only THE FIRST. Otherwise return all failures.
-- This may block indefinitely as it awaits either the end of the parser or its first success.
-- See 'suppressParser' if you want all successes.
isolateParser :: Monad m => ParserT l s e m a -> ParserT l s e m a
isolateParser = gatherParser True

-- | If the parser yields no results (success or failure), yield a given value.
defaultSuccessParser :: Monad m => a -> ParserT l s e m a -> ParserT l s e m a
defaultSuccessParser val parser = ParserT (\ls s -> ListT (go s (runParserT parser ls s))) where
  go s listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure (Just (ParseResult s (ParseValueSuccess val), empty))
      Just _ -> pure m

-- | If the parser yields no results (success or failure), throw the given error.
defaultErrorParser :: Monad m => e -> ParserT l s e m a -> ParserT l s e m a
defaultErrorParser e parser = ParserT (\ls s -> ListT (go ls s (runParserT parser ls s))) where
  go ls s listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure (Just (ParseResult s (ParseValueError (LabelledError ls (CompoundErrorCustom e))), empty))
      Just _ -> pure m

-- | A parser that yields 'Nothing' if there are no results (success or failure),
-- otherwise wrapping successes in 'Just'.
optionalParser :: Monad m => ParserT l s e m a -> ParserT l s e m (Maybe a)
optionalParser parser = defaultSuccessParser Nothing (fmap Just parser)

-- | Removes all failures from the parse results. Catches more errors than 'catchError (const empty)'
-- because this includes stream errors, not just custom errors.
-- If you want more fine-grained control, use 'reflectParser' and map over the results.
silenceParser :: Monad m => ParserT l s e m a -> ParserT l s e m a
silenceParser parser = ParserT (\ls s -> ListT (go (runParserT parser ls s))) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (res@(ParseResult _ v), rest) ->
        let nextListt = go rest
        in case v of
          ParseValueError _ -> nextListt
          ParseValueSuccess _ -> pure (Just (res, ListT nextListt))

-- | Yields the LONGEST string of 0 or more successes of the given parser (and passes through failures).
greedyStarParser :: (Chunked seq elem, Monad m) => ParserT l s e m elem -> ParserT l s e m seq
greedyStarParser parser = go [] where
  opt = optionalParser parser
  go !acc = do
    res <- opt
    case res of
      Nothing -> pure (revTokensToChunk acc)
      Just a -> go (consChunk a acc)

-- | Same as 'greedyStarParser' but discards the result.
greedyStarParser_ :: Monad m => ParserT l s e m a -> ParserT l s e m ()
greedyStarParser_ parser = go where
  opt = optionalParser parser
  go = do
    res <- opt
    case res of
      Nothing -> pure ()
      Just _ -> go

-- | Yields the LONGEST string of 1 or more successes of the given parser (and passes through failures).
greedyPlusParser :: (Chunked seq elem, Monad m) => ParserT l s e m elem -> ParserT l s e m seq
greedyPlusParser parser = liftA2 consChunk parser (greedyStarParser parser)

-- | Same as 'greedyPlusParser' but discards the result.
greedyPlusParser_ :: Monad m => ParserT l s e m a -> ParserT l s e m ()
greedyPlusParser_ parser = parser *> greedyStarParser_ parser

-- | Yield the results of the given parser, but rewind back to the starting state.
-- Note that these results may contain errors, so you may want to stifle them with 'silenceParser', for example.
lookAheadParser :: Monad m => ParserT l s e m a -> ParserT l s e m a
lookAheadParser parser = ParserT (\ls s -> ListT (go s (runParserT parser ls s))) where
  go s listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (ParseResult _ v, rest) -> pure (Just (ParseResult s v, ListT (go s rest)))

-- | Traverse the error and thrown state to change the error type of the parser.
traverseErrorParser :: Monad m => (s -> e -> m d) -> ParserT l s e m a -> ParserT l s d m a
traverseErrorParser f parser = ParserT (\ls s -> ListT (go (runParserT parser ls s))) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (ParseResult t v, rest) -> do
        w <- case v of
          ParseValueError (LabelledError ls ce) ->
            case ce of
              CompoundErrorStream se -> pure (ParseValueError (LabelledError ls (CompoundErrorStream se)))
              CompoundErrorCustom e -> fmap (ParseValueError . LabelledError ls . CompoundErrorCustom) (f t e)
          ParseValueSuccess a -> pure (ParseValueSuccess a)
        pure (Just (ParseResult t w, ListT (go rest)))

-- | Map over the error and thrown state to change the error type of the parser.
mapErrorParser :: Monad m => (s -> e -> d) -> ParserT l s e m a -> ParserT l s d m a
mapErrorParser f = traverseErrorParser (\e s -> pure (f e s))

-- | Drop all errors from the parse results. Unlike 'silenceParser', this
-- changes the error type of the parser so that no further errors can be thrown.
voidErrorParser :: Monad m => ParserT l s e m a -> ParserT l s Void m a
voidErrorParser parser = ParserT (\ls s -> runParserT parser ls s >>= go) where
  go (ParseResult t v) =
    case v of
      ParseValueError _ -> empty
      ParseValueSuccess a -> pure (ParseResult t (ParseValueSuccess a))
