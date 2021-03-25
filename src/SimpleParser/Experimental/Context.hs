module SimpleParser.Experimental.Context
  ( ContextStack (..)
  , pushContext
  , HasContextStack (..)
  , MonadContext
  , localPushContext
  , askContextStack
  ) where

import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.Trans (lift)
import Data.Sequence (Seq (..))
import ListT (ListT (..))
import qualified ListT
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

-- | Contstraint for those monads that can read a 'ContextStack'.
type MonadContext c r m = (HasContextStack c r, MonadReader r m)

-- | Push a label onto a context stack in the scope of a parser.
localPushContext :: MonadContext c r m => c -> ParserT e s m a -> ParserT e s m a
localPushContext label parser = ParserT (ListT . local (overContextStack (pushContext label)) . ListT.uncons . runParserT parser)

-- | Read a 'ContextStack'.
askContextStack :: MonadContext c r m => ParserT e s m (ContextStack c)
askContextStack = lift (asks viewContextStack)
