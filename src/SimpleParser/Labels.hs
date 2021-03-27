module SimpleParser.Labels
  ( LabelStack (..)
  , emptyLabelStack
  , pushLabel
  , HasLabelStack (..)
  , localPushLabel
  , askLabelStack
  ) where

import Control.Monad.Reader (MonadReader (..), asks)
import Data.Sequence (Seq (..))

-- | Stack of labels representing labels in the parse tree.
-- Behind the newtype, a "push" onto the stack is implemented as "snoc", therefore
-- fold/traverse goes from bottom of stack (most generic label) to top (most specific label).
newtype LabelStack l = LabelStack
  { unLabelStack :: Seq l
  } deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Easy constructor for the empty label stack
emptyLabelStack :: LabelStack l
emptyLabelStack = LabelStack Empty

-- | Pushes a label onto a 'LabelStack'
pushLabel :: l -> LabelStack l -> LabelStack l
pushLabel l = LabelStack . (:|> l) . unLabelStack

-- | Gets/sets a 'LabelStack' in an environment
class HasLabelStack l r | r -> l where
  viewLabelStack :: r -> LabelStack l
  setLabelStack :: LabelStack l -> r -> r
  overLabelStack :: (LabelStack l -> LabelStack l) -> r -> r
  overLabelStack f r = setLabelStack (f (viewLabelStack r)) r

instance HasLabelStack c (LabelStack c) where
  viewLabelStack = id
  setLabelStack = const
  overLabelStack = id

-- | Pushes a label onto a label stack in the scope of a parser
localPushLabel :: (HasLabelStack l r, MonadReader r m) => l -> m a -> m a
localPushLabel = local . overLabelStack . pushLabel

-- | Reads a 'LabelStack'
askLabelStack :: (HasLabelStack l r, MonadReader r m) => m (LabelStack l)
askLabelStack = asks viewLabelStack
