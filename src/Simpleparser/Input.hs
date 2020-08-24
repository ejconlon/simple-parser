module SimpleParser.Input
  ( InputT (..)
  , Input
  , runInputT
  , runInput
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (ReaderT (..), runReaderT)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (lift)
import Data.Maybe (isNothing)
import ListT (ListT (..))
import qualified ListT as ListT
import SimpleParser.Parser (ParserT (..))
import SimpleParser.Result (ParseResult, parseSuccessResult)
import SimpleParser.Stream (Stream, StreamT, runStreamT)

newtype InputT c e s m a = InputT
  { unInputT :: ReaderT (StreamT s m c) (ParserT e s m) a
  } deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadError e)

type Input c e s a = InputT c e s Identity a

runInputT :: InputT c e s m a -> StreamT s m c -> s -> ListT m (ParseResult e s a)
runInputT input stream startState = runParserT (runReaderT (unInputT input) stream) startState

runInput :: Input c e s a -> Stream s c -> s -> [ParseResult e s a]
runInput input stream startState = runIdentity (ListT.toList (runInputT input stream startState))

askInputStream :: Monad m => InputT c e s m (StreamT s m c)
askInputStream = InputT (ReaderT pure)

parserInput :: Monad m => ParserT e s m a -> InputT c e s m a
parserInput = InputT . lift

peekInput :: Monad m => InputT c e s m (Maybe c)
peekInput = do
  stream <- askInputStream
  parserInput $ ParserT $ \s -> ListT $ do
    m <- runStreamT stream s
    let v = maybe Nothing (Just . fst) m
    pure (Just (parseSuccessResult v s, empty))

popInput :: Monad m => InputT c e s m (Maybe c)
popInput = do
  stream <- askInputStream
  parserInput $ ParserT $ \s -> ListT $ do
    m <- runStreamT stream s
    let (v, t) = maybe (Nothing, s) (\(c, u) -> (Just c, u)) m
    pure (Just (parseSuccessResult v t, empty))

isEndInput :: Monad m => InputT c e s m Bool
isEndInput = isNothing <$> peekInput
