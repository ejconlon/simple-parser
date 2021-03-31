{-# LANGUAGE Rank2Types #-}

-- | 'ParserT' is the core monad transformer for parsing.
module SimpleParser.Parser
  ( ParserT (..)
  , Parser
  , runParser
  , pureParser
  , bindParser
  , failParser
  , liftParser
  , hoistParser
  , catchJustParser
  , throwParser
  , catchParser
  , emptyParser
  , orParser
  , greedyStarParser
  , greedyStarParser_
  , greedyPlusParser
  , greedyPlusParser_
  , defaultParser
  , optionalParser
  , silenceParser
  , lookAheadParser
  , markParser
  , markWithStateParser
  , markWithOptStateParser
  , unmarkParser
  , commitParser
  , onEmptyParser
  ) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad (MonadPlus (..), ap, (>=>))
import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (first)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty ((><|))
import qualified Data.Sequence.NonEmpty as NESeq
import Data.Text (Text)
import qualified Data.Text as T
import SimpleParser.Chunked (Chunked (..))
import SimpleParser.Result (CompoundError (..), Mark (..), ParseError (..), ParseResult (..), ParseSuccess (..),
                            markParseError, parseErrorResume, unmarkParseError)
import SimpleParser.Stack (emptyStack)

-- | A 'ParserT' is a state/error/list transformer useful for parsing.
-- All MTL instances are for this transformer only. If, for example, your effect
-- has its own 'MonadState' instance, you'll have to use 'lift get' instead of 'get'.
newtype ParserT l s e m a = ParserT { runParserT :: s -> m (Maybe (ParseResult l s e a)) }
  deriving (Functor)

-- | Use 'Parser' if you have no need for other monadic effects.
type Parser l s e a = ParserT l s e Identity a

-- | Runs a non-effectful parser from an inital state and collects all results.
runParser :: Parser l s e a -> s -> Maybe (ParseResult l s e a)
runParser parser s = runIdentity (runParserT parser s)

-- | Applicative pure
pureParser :: Monad m => a -> ParserT l s e m a
pureParser a = ParserT (\s -> pure (Just (ParseResultSuccess (ParseSuccess s a))))

instance Monad m => Applicative (ParserT l s e m) where
  pure = pureParser
  (<*>) = ap

-- | Monadic bind
bindParser :: Monad m => ParserT l s e m a -> (a -> ParserT l s e m b) -> ParserT l s e m b
bindParser parser f = ParserT (runParserT parser >=> go) where
  go mres =
    case mres of
      Nothing -> pure Nothing
      Just res ->
          case res of
            ParseResultError errs -> pure (Just (ParseResultError errs))
            ParseResultSuccess (ParseSuccess t a) -> runParserT (f a) t

instance Monad m => Monad (ParserT l s e m) where
  return = pure
  (>>=) = bindParser

-- | The empty parser
emptyParser :: Monad m => ParserT l s e m a
emptyParser = ParserT (const (pure Nothing))

-- | Yields from the first parser of the two that returns a successfull result.
-- Otherwise will merge and yield all errors.
orParser :: Monad m => ParserT l s e m a -> ParserT l s e m a -> ParserT l s e m a
orParser one two = ParserT (\s -> runParserT one s >>= go1 s) where
  go1 s mres1 =
    case mres1 of
      Nothing -> runParserT two s >>= go2 empty
      Just res1 ->
        case res1 of
          ParseResultSuccess _ -> pure mres1
          ParseResultError es1 -> runParserT two s >>= go2 (NESeq.toSeq es1)

  go2 es1 mres2 =
    case mres2 of
      Nothing -> pure (fmap ParseResultError (NESeq.nonEmptySeq es1))
      Just res2 ->
        case res2 of
          ParseResultSuccess _ -> pure mres2
          ParseResultError es2 -> pure (Just (ParseResultError (es1 ><| es2)))

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

instance Monad m => Alternative (ParserT l s e m) where
  empty = emptyParser
  (<|>) = orParser
  some = greedyPlusParser
  many = greedyStarParser

instance Monad m => MonadPlus (ParserT l s e m) where
  mzero = empty
  mplus = (<|>)

instance Monad m => MonadState s (ParserT l s e m) where
  get = ParserT (\s -> pure (Just (ParseResultSuccess (ParseSuccess s s))))
  put t = ParserT (\_ -> pure (Just (ParseResultSuccess (ParseSuccess t ()))))
  state f = ParserT (\s -> let (!a, !t) = f s in pure (Just (ParseResultSuccess (ParseSuccess t a))))

-- | Catch only a subset of custom errors. This preserves label information vs rethrowing.
catchJustParser :: Monad m => (e -> Maybe b) -> ParserT l s e m a -> (b -> ParserT l s e m a) -> ParserT l s e m a
catchJustParser filterer parser handler = ParserT (runParserT parser >=> go) where
    go mres =
      case mres of
        Nothing -> pure Nothing
        Just res ->
          case res of
            ParseResultSuccess _ ->
              -- Nothing to catch, yield existing success
              pure mres
            ParseResultError es ->
              -- Find first custom error to handle
              goSplit Empty (NESeq.toSeq es)

    goSplit beforeEs afterEs =
      case seqPartition extractCustomError afterEs of
        Nothing ->
          -- No next custom error, finally yield all other errors
          pure (maybe empty (pure . ParseResultError) (NESeq.nonEmptySeq (beforeEs <> afterEs)))
        Just sep ->
          -- Found custom error - handle it
          goHandle beforeEs sep

    goHandle beforeEs (SeqPartition nextBeforeEs targetE (s, e) afterEs) =
      case filterer e of
        Nothing ->
          -- Not handling error;  - find next custom error
          goSplit (beforeEs <> (targetE :<| nextBeforeEs)) afterEs
        Just b -> do
          mres <- runParserT (handler b) s
          case mres of
            Nothing ->
              -- No results from handled error - find next custom error
              goSplit (beforeEs <> nextBeforeEs) afterEs
            Just res ->
              case res of
                ParseResultSuccess _ -> pure mres
                ParseResultError es ->
                  -- Add to list of errors and find next custom error
                  goSplit (beforeEs <> nextBeforeEs <> NESeq.toSeq es) afterEs

-- | Throws a custom error
throwParser :: Monad m => e -> ParserT l s e m a
throwParser e = ParserT (\s -> pure (Just (ParseResultError (pure (ParseError emptyStack s (CompoundErrorCustom e))))))

-- | Catches a custom error
catchParser :: Monad m => ParserT l s e m a -> (e -> ParserT l s e m a) -> ParserT l s e m a
catchParser = catchJustParser Just

instance Monad m => MonadError e (ParserT l s e m) where
  throwError = throwParser
  catchError = catchJustParser Just

-- | A simple failing parser
failParser :: Monad m => Text -> ParserT l s e m a
failParser msg = ParserT (\s -> pure (Just (ParseResultError (pure (ParseError emptyStack s (CompoundErrorFail msg))))))

instance Monad m => MonadFail (ParserT l s e m) where
  fail = failParser . T.pack

liftParser :: Monad m => m a -> ParserT l s e m a
liftParser ma = ParserT (\s -> fmap (Just . ParseResultSuccess . ParseSuccess s) ma)

instance MonadTrans (ParserT l s e) where
  lift = liftParser

hoistParser :: (forall x. m x -> n x) -> ParserT l s e m a -> ParserT l s e n a
hoistParser trans (ParserT f) = ParserT (trans . f)

instance MFunctor (ParserT l s e) where
  hoist = hoistParser

-- | If the parser does not succeed, yield the given value.
defaultParser :: Monad m => a -> ParserT l s e m a -> ParserT l s e m a
defaultParser val parser = orParser parser (pure val)

-- | A parser that yields 'Nothing' if the parser does not succeed, otherwise
-- wraps success in 'Just'.
optionalParser :: Monad m => ParserT l s e m a -> ParserT l s e m (Maybe a)
optionalParser parser = defaultParser Nothing (fmap Just parser)

-- | Removes all failures from the parse results. Catches more errors than 'catchError (const empty)'
-- because this includes stream errors, not just custom errors.
-- If you want more fine-grained control, use 'reflectParser' and map over the results.
silenceParser :: Monad m => ParserT l s e m a -> ParserT l s e m a
silenceParser parser = ParserT (fmap (>>= go) . runParserT parser) where
  go res =
    case res of
      ParseResultError _ -> Nothing
      ParseResultSuccess _ -> Just res

-- | Yield the results of the given parser, but rewind back to the starting state in ALL cases (success and error).
-- Note that these results may contain errors, so you may want to stifle them with 'silenceParser', for example.
lookAheadParser :: Monad m => Maybe l -> ParserT l s e m a -> ParserT l s e m a
lookAheadParser ml parser = ParserT (\s -> fmap (fmap (go s)) (runParserT parser s)) where
  go s res =
    case res of
      ParseResultError es -> ParseResultError (fmap (markParseError (Mark ml s)) es)
      ParseResultSuccess (ParseSuccess _ a) -> ParseResultSuccess (ParseSuccess s a)

-- | Yield the results of the given parser, but rewind back to the starting state on error ONLY.
markParser :: Monad m => Maybe l -> ParserT l s e m a -> ParserT l s e m a
markParser ml parser = ParserT (\s -> fmap (fmap (go s)) (runParserT parser s)) where
  go s res =
    case res of
      ParseResultError es -> ParseResultError (fmap (markParseError (Mark ml s)) es)
      ParseResultSuccess _ -> res

-- | Like 'markParser' but allows you to mutate state. See 'withToken' and 'withChunk'.
markWithStateParser :: Monad m => Maybe l -> (s -> (b, s)) -> (b -> ParserT l s e m a) -> ParserT l s e m a
markWithStateParser ml g f = markParser ml (state g >>= f)

-- | Like 'markParser' but allows you to mutate state. See 'withToken' and 'withChunk'.
markWithOptStateParser :: Monad m => Maybe l -> (s -> Maybe (b, s)) -> (Maybe b -> ParserT l s e m a) -> ParserT l s e m a
markWithOptStateParser ml g = markWithStateParser ml (\s -> maybe (Nothing, s) (first Just) (g s))

-- | Clear marks from parse errors. You can mark immediately after to widen the narrowest
-- marked span to the range you want to report.
unmarkParser :: Monad m => ParserT l s e m a -> ParserT l s e m a
unmarkParser parser = ParserT (fmap (fmap go) . runParserT parser) where
  go res =
    case res of
      ParseResultError es -> ParseResultError (fmap unmarkParseError es)
      ParseResultSuccess _ -> res

-- | If the first parser succeeds in the given state, yield results from the second parser in the given
-- state. This is likely the look-ahead you want. Use the first parser to check a prefix of input,
-- and use the second to consume that input.
commitParser :: Monad m => ParserT l s e m () -> ParserT l s e m a -> ParserT l s e m a
commitParser checker parser = do
  s <- get
  o <- optionalParser checker
  case o of
    Nothing -> empty
    Just _ -> put s *> parser

-- | If the first parser yields NO results (success or failure), yield from the second.
-- Note that this is different from 'orParser' in that it does not try the second if there
-- are errors in the first. You might use this on the outside of a complex parser with
-- a fallback to 'fail' to indicate that there are no matches.
onEmptyParser :: Parser l s e a -> Parser l s e a -> Parser l s e a
onEmptyParser parser fallback = ParserT (\s -> runParserT parser s >>= go s) where
  go s mres =
    case mres of
      Nothing -> runParserT fallback s
      Just _ -> pure mres

-- Private utility functions

data SeqPartition a b = SeqPartition
  { spBefore :: !(Seq a)
  , spKey :: !a
  , spValue :: !b
  , spAfter :: !(Seq a)
  } deriving (Eq, Show)

seqPartition :: (a -> Maybe b) -> Seq a -> Maybe (SeqPartition a b)
seqPartition f = go Empty where
  go before after =
    case after of
      Empty -> Nothing
      (x :<| xs) ->
        case f x of
          Nothing -> go (before :|> x) xs
          Just y -> Just (SeqPartition before x y xs)

extractCustomError :: ParseError l s e -> Maybe (s, e)
extractCustomError pe@(ParseError _ _ ce) =
  case ce of
    CompoundErrorCustom e -> Just (parseErrorResume pe, e)
    _ -> Nothing
