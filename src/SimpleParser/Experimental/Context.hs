module SimpleParser.Experimental.Context
  ( ContextStack (..)
  , pushContext
  , HasContextStack (..)
  , localPushContext
  , askContextStack
  ) where

import Control.Monad.Reader (MonadReader (..), asks)
import Data.Sequence (Seq (..))
import SimpleParser.Parser (ParserT (..))

-- | Stack of labels representing contexts in the parse tree.
newtype ContextStack c = ContextStack
  { unContextStack :: Seq c
  } deriving (Eq, Show)

-- | Push a label onto a 'ContextStack'
pushContext :: c -> ContextStack c -> ContextStack c
pushContext c = ContextStack . (c :<|) . unContextStack

-- | Get/set a 'ContextStack' in an environment.
class HasContextStack c r | r -> c where
  viewContextStack :: r -> ContextStack c
  setContextStack :: ContextStack c -> r -> r
  overContextStack :: (ContextStack c -> ContextStack c) -> r -> r
  overContextStack f r = setContextStack (f (viewContextStack r)) r

instance HasContextStack c (ContextStack c) where
  viewContextStack = id
  setContextStack = const
  overContextStack = id

-- | Push a label onto a context stack in the scope of a parser.
localPushContext :: (HasContextStack c r, Monad m) => c -> ParserT r s e m a -> ParserT r s e m a
localPushContext = local . overContextStack . pushContext

-- | Read a 'ContextStack'.
askContextStack :: (HasContextStack c r, Monad m) => ParserT r s e m (ContextStack c)
askContextStack = asks viewContextStack
