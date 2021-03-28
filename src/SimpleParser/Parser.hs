-- | 'ParserT' is the core monad transformer for parsing.
module SimpleParser.Parser
  ( ParserT (..)
  , Parser
  , runParser
  , emptyParser
  , failParser
  , catchJustParser
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
  , optionalParser
  , silenceParser
  , greedyStarParser
  , greedyStarParser_
  , greedyPlusParser
  , greedyPlusParser_
  , lookAheadParser
  , markParser
  , markWithStateParser
  , markWithOptStateParser
  , mapErrorParser
  , traverseErrorParser
  , labelParser
  , fastForwardParser
  ) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad (MonadPlus (..), ap)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import qualified Data.Text as T
import ListT (ListT (..))
import qualified ListT
import SimpleParser.Chunked (Chunked (..))
import SimpleParser.Labels (LabelStack (..), localPushLabel)
import SimpleParser.Result (CompoundError (..), ParseError (..), ParseResult (..), ParseValue (..))

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
  empty = emptyParser
  (<|>) = orParser
  some = greedyPlusParser
  many = greedyStarParser

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
  throwError e = ParserT (\ls s -> pure (ParseResult s (ParseValueError (ParseError ls s (CompoundErrorCustom e)))))
  catchError parser handler = ParserT (\ls s -> runParserT parser ls s >>= go ls) where
    go ls res@(ParseResult t v) =
      case v of
        ParseValueError (ParseError _ _ ce) ->
          case ce of
            CompoundErrorCustom e -> runParserT (handler e) ls t
            _ -> pure res
        ParseValueSuccess a -> pure (ParseResult t (ParseValueSuccess a))

instance Monad m => MonadFail (ParserT l s e m) where
  fail = failParser . T.pack

instance MonadTrans (ParserT l s e) where
  lift ma = ParserT (\_ s -> lift (fmap (ParseResult s . ParseValueSuccess) ma))

instance MFunctor (ParserT l s e) where
  hoist trans (ParserT f) = ParserT (\ls s -> hoist trans (f ls s))

-- | Runs a non-effectful parser from an inital state and collects all results.
runParser :: Parser l s e a -> LabelStack l -> s -> [ParseResult l s e a]
runParser parser ls s = runIdentity (ListT.toList (runParserT parser ls s))

-- | The empty parser
emptyParser :: Monad m => ParserT l s e m a
emptyParser = ParserT (\_ _ -> ListT (pure Nothing))

-- | A simple failing parser
failParser :: Monad m => Text -> ParserT l s e m a
failParser msg = ParserT (\ls s -> pure (ParseResult s (ParseValueError (ParseError ls s (CompoundErrorFail msg)))))

-- | Catch only a subset of custom errors. This preserves label information vs rethrowing.
catchJustParser :: Monad m => (e -> Maybe a) -> ParserT l s e m a -> ParserT l s e m a
catchJustParser handler parser = ParserT (\ls s -> ListT (go (runParserT parser ls s))) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (res@(ParseResult s v), rest) ->
        let nextListt = ListT (go rest)
        in case v of
          ParseValueError (ParseError _ _ (CompoundErrorCustom e)) ->
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
reflectParser parser = ParserT (\ls s -> fmap go (runParserT parser ls s)) where
  go (ParseResult t v) = ParseResult t (ParseValueSuccess v)

-- | Dangerous function that lets you insert values as given into the parser.
-- There is no guarantee that the labels are reasonable!
-- However, this is still exposed as a back door in case you really want to
-- reflect without observable effect. (We could write this as a fancy bind
-- but the parser type is not that protected anyway.)
unreflectParser :: Monad m => ParseValue l s e a -> ParserT l s e m a
unreflectParser v = ParserT (\_ s -> pure (ParseResult s v))

-- -- | Combines the results of two parsers. Equivalent to '<|>'
andParser :: Monad m => ParserT l s e m a -> ParserT l s e m a -> ParserT l s e m a
andParser one two = ParserT (\ls s -> ListT (go ls s (runParserT one ls s))) where
  go r s listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> ListT.uncons (runParserT two r s)
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

-- | Yields ALL results from the FIRST parser of the two that returns any SUCCESSFUL results (success or failure).
-- If no successful results, combined failures are returned.
orParser :: Monad m => ParserT l s e m a -> ParserT l s e m a -> ParserT l s e m a
orParser one two = ParserT (\ls s -> ListT (go1 ls s Empty (runParserT one ls s))) where
  go1 ls s !racc1 listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> go2 racc1 Empty (runParserT two ls s)
      Just (res@(ParseResult _ v), rest) ->
        case v of
          ParseValueError _ -> go1 ls s (racc1 :|> res) rest
          ParseValueSuccess _ -> ret racc1 m

  go2 !racc1 !racc2 listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> ret (racc1 <> racc2) Nothing
      Just (res@(ParseResult _ v), rest) ->
        case v of
          ParseValueError _ -> go2 racc1 (racc2 :|> res) rest
          ParseValueSuccess _ -> ret racc2 m

  ret !racc m = do
    case racc of
      Empty -> pure m
      res :<| racc' -> pure (Just (res, ListT (ret racc' m)))

-- | Yields ALL results from the FIRST parser of the list that returns any SUCCESSFUL results (success or failure).
-- If no successful results, combined failures are returned.
orAllParser :: (Monad m, Foldable f) => f (ParserT l s e m a) -> ParserT l s e m a
orAllParser = start . toList where
  start ps =
    case ps of
      [] -> empty
      q:qs -> ParserT (\ls s -> ListT (run ls s qs Empty Empty (runParserT q ls s)))

  run r s qs !raccAll !racc listt = do
    m <- ListT.uncons listt
    case m of
      Nothing ->
        case qs of
          [] -> ret (raccAll <> racc) Nothing
          x:xs -> run r s xs (raccAll <> racc) Empty (runParserT x r s)
      Just (res@(ParseResult _ v), rest) ->
        case v of
          ParseValueError _ -> run r s qs raccAll (racc :|> res) rest
          ParseValueSuccess _ -> ret racc m

  ret !racc m = do
    case racc of
      Empty -> pure m
      res :<| racc' -> pure (Just (res, ListT (ret racc' m)))

-- Private: used for 'suppressParser' and 'isolateParser'.
-- Buffer errors until a success is found:
-- Return first success if single is true.
-- Return all successes if single is false.
-- Return all errors if no success is found.
gatherParser :: Monad m => Bool -> ParserT l s e m a -> ParserT l s e m a
gatherParser single parser = ParserT (\ls s -> ListT (go Empty (runParserT parser ls s))) where
  go !racc listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> returnErr racc
      Just (res@(ParseResult _ v), rest) ->
        case v of
          ParseValueError _ -> go (racc :|> res) rest
          ParseValueSuccess _ ->
            let nextListt = if single then empty else ListT (filterOk rest)
            in pure (Just (res, nextListt))

  returnErr racc =
    case racc of
      Empty -> pure Nothing
      res :<| rs -> pure (Just (res, ListT (returnErr rs)))

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

-- | If the parser yields NO SUCCESSES, yield the given value.
defaultParser :: Monad m => a -> ParserT l s e m a -> ParserT l s e m a
defaultParser val parser = orParser parser (pure val)
-- TODO(ejconlon) find some way to avoid gathering errors in orParser
-- since we know we won't need them

-- | A parser that yields 'Nothing' if there are NO SUCCESSES, otherwise
-- wraps successes in 'Just' and passes through errors.
optionalParser :: Monad m => ParserT l s e m a -> ParserT l s e m (Maybe a)
optionalParser parser = defaultParser Nothing (fmap Just parser)

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

-- | Yields the LONGEST string of 0 or more successes of the given parser.
-- Failures will be silenced.
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

-- | Yields the LONGEST string of 1 or more successes of the given parser.
-- Failures in the tail will be silenced, but those in the head will be returned.
greedyPlusParser :: (Chunked seq elem, Monad m) => ParserT l s e m elem -> ParserT l s e m seq
greedyPlusParser parser = liftA2 consChunk parser (greedyStarParser parser)

-- | Same as 'greedyPlusParser' but discards the result.
greedyPlusParser_ :: Monad m => ParserT l s e m a -> ParserT l s e m ()
greedyPlusParser_ parser = parser *> greedyStarParser_ parser

-- | Yield the results of the given parser, but rewind back to the starting state in ALL cases (success and error).
-- Note that these results may contain errors, so you may want to stifle them with 'silenceParser', for example.
lookAheadParser :: Monad m => ParserT l s e m a -> ParserT l s e m a
lookAheadParser parser = ParserT (\ls s -> fmap (go s) (runParserT parser ls s)) where
  go s (ParseResult _ v) = ParseResult s v

-- | Yield the results of the given parser, but rewind back to the starting state on error ONLY.
markParser :: Monad m => ParserT l s e m a -> ParserT l s e m a
markParser parser = ParserT (\ls s -> fmap (go s) (runParserT parser ls s)) where
  go s res@(ParseResult _ v) =
    case v of
      ParseValueError _ -> ParseResult s v
      ParseValueSuccess _ -> res

-- | Like 'markParser' but allows you to mutate state. See 'withToken' and 'withChunk'.
markWithStateParser :: Monad m => (s -> (b, s)) -> (b -> ParserT l s e m a) -> ParserT l s e m a
markWithStateParser g f = markParser (state g >>= f)

-- | Like 'markParser' but allows you to mutate state. See 'withToken' and 'withChunk'.
markWithOptStateParser :: Monad m => (s -> Maybe (b, s)) -> (Maybe b -> ParserT l s e m a) -> ParserT l s e m a
markWithOptStateParser g = markWithStateParser (\s -> maybe (Nothing, s) (first Just) (g s))

-- | Traverse the error and thrown state to change the error type of the parser.
traverseErrorParser :: Monad m => (s -> e -> m d) -> ParserT l s e m a -> ParserT l s d m a
traverseErrorParser f parser = ParserT (\ls s -> ListT (go (runParserT parser ls s))) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (ParseResult t v, rest) -> do
        w <- case v of
          ParseValueError (ParseError ls es ce) -> fmap (ParseValueError . ParseError ls es) mce where
            mce = case ce of
              CompoundErrorStream se -> pure (CompoundErrorStream se)
              CompoundErrorFail msg -> pure (CompoundErrorFail msg)
              CompoundErrorCustom e -> fmap CompoundErrorCustom (f t e)
          ParseValueSuccess a -> pure (ParseValueSuccess a)
        pure (Just (ParseResult t w, ListT (go rest)))

-- | Map over the error and thrown state to change the error type of the parser.
mapErrorParser :: Monad m => (s -> e -> d) -> ParserT l s e m a -> ParserT l s d m a
mapErrorParser f = traverseErrorParser (\e s -> pure (f e s))

-- | Push the given label onto the stack to annotate errors in the given parser.
labelParser :: Monad m => l -> ParserT l s e m a -> ParserT l s e m a
labelParser = localPushLabel

-- | Skip to the END of any errors. Be careful - this may leave you in strange places!
fastForwardParser :: Monad m => ParserT l s e m a -> ParserT l s e m a
fastForwardParser parser = ParserT (\ls s -> fmap go (runParserT parser ls s)) where
  go res@(ParseResult _ v) =
    case v of
      ParseValueError (ParseError _ es _) -> ParseResult es v
      ParseValueSuccess _ -> res
