module SimpleParser.Input
  ( InputT (..)
  , Input
  , runInputT
  , runInput
  , parseInput
  , peekInput
  , popInput
  , isEndInput
  , endInput
  , skipInput
  , satisfyInput
  , foldInputWhile
  , takeInputWhile
  , dropInputWhile
  , charInput
  , wordInput
  , wordInput_
  , adaptInput
  , filterInput
  , reflectInput
  , branchInput
  , suppressInput
  , defaultInput
  , optionalInput
  , silenceInput
  , greedyStarInput
  , greedyStarInput_
  , greedyPlusInput
  , greedyPlusInput_
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Control.Monad.Except (MonadError)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader, ReaderT (..), runReaderT)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Maybe (isNothing)
import ListT (ListT (..))
import qualified ListT
import SimpleParser.Parser (ParserT (..), branchParser, defaultParser, filterParser, greedyPlusParser,
                            greedyPlusParser_, greedyStarParser, greedyStarParser_, optionalParser, reflectParser,
                            silenceParser, suppressParser)
import SimpleParser.Result (ParseResult (..), ParseValue (..))
import SimpleParser.Stream (Stream, StreamT, runStreamT)

newtype InputT c e s m a = InputT
  { unInputT :: ReaderT (StreamT s m c) (ParserT e s m) a
  } deriving (Functor, Applicative, Monad, Alternative, MonadPlus,
      MonadError e, MonadState s, MonadReader (StreamT s m c))

type Input c e s a = InputT c e s Identity a

instance MonadTrans (InputT c e s) where
  lift = InputT . lift . lift

runInputT :: InputT c e s m a -> StreamT s m c -> s -> ListT m (ParseResult e s a)
runInputT input stream = runParserT (runReaderT (unInputT input) stream)

runInput :: Input c e s a -> Stream s c -> s -> [ParseResult e s a]
runInput input stream startState = runIdentity (ListT.toList (runInputT input stream startState))

parseInput :: Monad m => ParserT e s m a -> InputT c e s m a
parseInput = InputT . lift

peekInput :: Monad m => InputT c e s m (Maybe c)
peekInput = InputT $ ReaderT $ \stream ->
  ParserT $ \s -> ListT $ do
    m <- runStreamT stream s
    pure (Just (ParseResult (ParseSuccess (fmap fst m)) s, empty))

popInput :: Monad m => InputT c e s m (Maybe c)
popInput = InputT $ ReaderT $ \stream ->
  ParserT $ \s -> ListT $ do
    m <- runStreamT stream s
    let (v, t) = maybe (Nothing, s) (first Just) m
    pure (Just (ParseResult (ParseSuccess v) t, empty))

isEndInput :: Monad m => InputT c e s m Bool
isEndInput = isNothing <$> peekInput

endInput :: Monad m => InputT c e s m ()
endInput = do
  m <- peekInput
  maybe (pure ()) (const empty) m

skipInput :: Monad m => InputT c e s m c
skipInput = do
  m <- popInput
  maybe empty pure m

satisfyInput :: Monad m => (c -> Bool) -> InputT c e s m c
satisfyInput p = do
  m <- popInput
  case m of
    Just c | p c -> pure c
    _ -> empty

foldInputWhile :: Monad m => (c -> x -> (Bool, x)) -> x -> InputT c e s m x
foldInputWhile f = go where
  go !x = do
    m <- popInput
    case m of
      Nothing -> pure x
      Just c ->
        let (ok, newX) = f c x
        in if ok
          then go newX
          else pure x

takeInputWhile :: Monad m => (c -> Bool) -> InputT c e s m [c]
takeInputWhile pcate = fmap reverse (foldInputWhile append []) where
  append x xs = if pcate x then (True, x:xs) else (False, xs)

dropInputWhile :: Monad m => (c -> Bool) -> InputT c e s m ()
dropInputWhile pcate = go where
  go = do
    m <- popInput
    case m of
      Just c | pcate c -> go
      _ -> pure ()

charInput :: (Monad m, Eq c) => c -> InputT c e s m c
charInput = satisfyInput . (==)

wordInput :: (Monad m, Eq c, Foldable f) => f c -> InputT c e s m [c]
wordInput = go [] . toList where
  go !acc cs =
    case cs of
      [] -> pure (reverse acc)
      (c:cs') -> do
        c' <- charInput c
        go (c':acc) cs'

wordInput_ :: (Monad m, Eq c, Foldable f) => f c -> InputT c e s m ()
wordInput_ = go . toList where
  go cs =
    case cs of
      [] -> pure ()
      (c:cs') -> charInput c >> go cs'

adaptInput :: (ParserT e s m a -> ParserT e s m b) -> InputT c e s m a -> InputT c e s m b
adaptInput f input = InputT (ReaderT (f . ParserT . runInputT input))

filterInput :: Monad m => (a -> Bool) -> InputT c e s m a -> InputT c e s m a
filterInput = adaptInput . filterParser

reflectInput :: Monad m => InputT c e s m a -> InputT c e s m (ParseValue e a)
reflectInput = adaptInput reflectParser

branchInput :: (Foldable f, Monad m) => f (InputT c e s m a) -> InputT c e s m a
branchInput inputs = InputT (ReaderT (\stream -> branchParser (fmap (ParserT . flip runInputT stream) (toList inputs))))

suppressInput :: Monad m => InputT c e s m a -> InputT c e s m a
suppressInput = adaptInput suppressParser

defaultInput :: Monad m => a -> InputT c e s m a -> InputT c e s m a
defaultInput = adaptInput . defaultParser

optionalInput :: Monad m => InputT c e s m a -> InputT c e s m (Maybe a)
optionalInput = adaptInput optionalParser

silenceInput :: Monad m => InputT c e s m a -> InputT c e s m a
silenceInput = adaptInput silenceParser

greedyStarInput :: Monad m => InputT c e s m a -> InputT c e s m [a]
greedyStarInput = adaptInput greedyStarParser

greedyStarInput_ :: Monad m => InputT c e s m a -> InputT c e s m ()
greedyStarInput_ = adaptInput greedyStarParser_

greedyPlusInput :: Monad m => InputT c e s m a -> InputT c e s m [a]
greedyPlusInput = adaptInput greedyPlusParser

greedyPlusInput_ :: Monad m => InputT c e s m a -> InputT c e s m ()
greedyPlusInput_ = adaptInput greedyPlusParser_
