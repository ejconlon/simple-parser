module SimpleParser.Context
  ( ContextStack (..)
  , pushContext
  , HasContextStack (..)
  , MonadContext
  , localPushContext
  , askContextStack
  ) where

import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.Trans (lift)
import Data.Sequence (Seq)
import ListT (ListT (..))
import qualified ListT
import SimpleParser.Parser (ParserT (..))

newtype ContextStack c = ContextStack
  { unContextStack :: Seq c
  } deriving (Eq, Show)

pushContext :: c -> ContextStack c -> ContextStack c
pushContext = undefined

class HasContextStack c r | r -> c where
  viewContextStack :: r -> ContextStack c
  setContextStack :: ContextStack c -> r -> r
  overContextStack :: (ContextStack c -> ContextStack c) -> r -> r
  overContextStack f r = setContextStack (f (viewContextStack r)) r

instance HasContextStack c (ContextStack c) where
  viewContextStack = id
  setContextStack = const
  overContextStack = id

type MonadContext c r m = (HasContextStack c r, MonadReader r m)

localPushContext :: MonadContext c r m => c -> ParserT e s m a -> ParserT e s m a
localPushContext label parser = ParserT (ListT . local (overContextStack (pushContext label)) . ListT.uncons . runParserT parser)

askContextStack :: MonadContext c r m => ParserT e s m (ContextStack c)
askContextStack = lift (asks viewContextStack)
