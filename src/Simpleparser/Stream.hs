module SimpleParser.Stream
  ( StreamT (..)
  , Stream
  , runStream
  , ListStreamState (..)
  , StringStreamState
  , newListStreamState
  , newStringStreamState
  , listStream
  , stringStream
  ) where

import Control.Monad.Identity

newtype StreamT s m a = StreamT
  { runStreamT :: s -> m (Maybe (a, s))
  } deriving (Functor)

type Stream s a = StreamT s Identity a

runStream :: Stream s a -> s -> Maybe (a, s)
runStream stream startState = runIdentity (runStreamT stream startState)

data ListStreamState a = ListStreamState
  { lssOffset :: !Int
  , lssContents :: ![a]
  } deriving (Eq, Show, Functor, Foldable, Traversable)

newListStreamState :: [a] -> ListStreamState a
newListStreamState = ListStreamState 0

type StringStreamState = ListStreamState Char

newStringStreamState :: String -> StringStreamState
newStringStreamState = newListStreamState

unStreamList :: ListStreamState a -> Identity (Maybe (a, ListStreamState a))
unStreamList (ListStreamState o as) =
  case as of
    [] -> pure Nothing
    b:bs -> pure (Just (b, ListStreamState (succ o) bs))

listStream :: Stream (ListStreamState a) a
listStream = StreamT unStreamList

stringStream :: Stream StringStreamState Char
stringStream = listStream
