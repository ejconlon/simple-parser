module SimpleParser.Stream
  ( StreamT
  , Stream
  , runStreamT
  , runStream
  , ListStreamState
  , StringStreamState
  , listStream
  , stringStream
  ) where

import Control.Monad.Identity

-- TODO StateT MaybeT
newtype StreamT s m a = StreamT
  { runStreamT :: s -> m (Maybe (a, s))
  } deriving (Functor)

type Stream s a = StreamT s Identity a

runStream :: Stream s a -> s -> Maybe (a, s)
runStream stream startState = runIdentity (runStreamT stream startState)

data ListStreamState a = ListStreamState
  { lssContents :: ![a]
  , lssOffset :: !Int
  } deriving (Functor, Foldable, Traversable)

type StringStreamState = ListStreamState Char

unStreamList :: ListStreamState a -> Identity (Maybe (a, ListStreamState a))
unStreamList (ListStreamState as o) =
  case as of
    [] -> pure Nothing
    b:bs -> pure (Just (b, ListStreamState bs (succ o)))

listStream :: Stream (ListStreamState a) a
listStream = StreamT unStreamList

stringStream :: Stream StringStreamState Char
stringStream = listStream
