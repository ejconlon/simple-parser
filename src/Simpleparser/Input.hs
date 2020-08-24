module SimpleParser.Input
  ( InputT (..)
  , Input
  , runInputT
  , runInput
  , parseInput
  , peekInput
  , popInput
  , isEndInput
  , branchInput
  , suppressInput
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Control.Monad.Except (MonadError)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (ReaderT (..), runReaderT)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Maybe (isNothing)
import ListT (ListT (..))
import qualified ListT
import SimpleParser.Parser (ParserT (..), branchParser, suppressParser)
import SimpleParser.Result (ParseResult (..), ParseValue (..))
import SimpleParser.Stream (Stream, StreamT, runStreamT)

newtype InputT c e s m a = InputT
  { unInputT :: ReaderT (StreamT s m c) (ParserT e s m) a
  } deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadError e)

type Input c e s a = InputT c e s Identity a

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

branchInput :: (Foldable f, Monad m) => f (InputT c e s m a) -> InputT c e s m a
branchInput inputs = InputT (ReaderT (\stream -> branchParser (fmap (ParserT . flip runInputT stream) (toList inputs))))

suppressInput :: Monad m => InputT c e s m a -> InputT c e s m a
suppressInput input = InputT (ReaderT (suppressParser . ParserT . runInputT input))
