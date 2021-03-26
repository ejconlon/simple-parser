module SimpleParser.Labels
  ( LabelStack (..)
  , emptyLabelStack
  , pushLabel
  , HasLabelStack (..)
  , localPushLabel
  , askLabelStack
  , LabelledError (..)
  , labelledError
  , unlabelledError
  ) where

import Control.Monad.Reader (MonadReader (..), asks)
import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

-- | Stack of labels representing labels in the parse tree.
newtype LabelStack l = LabelStack
  { unLabelStack :: Seq l
  } deriving (Eq, Show, Functor, Foldable, Traversable)

emptyLabelStack :: LabelStack l
emptyLabelStack = LabelStack Empty

-- | Push a label onto a 'LabelStack'
pushLabel :: l -> LabelStack l -> LabelStack l
pushLabel l = LabelStack . (l :<|) . unLabelStack

-- | Gets/sets a 'LabelStack' in an environment.
class HasLabelStack l r | r -> l where
  viewLabelStack :: r -> LabelStack l
  setLabelStack :: LabelStack l -> r -> r
  overLabelStack :: (LabelStack l -> LabelStack l) -> r -> r
  overLabelStack f r = setLabelStack (f (viewLabelStack r)) r

instance HasLabelStack c (LabelStack c) where
  viewLabelStack = id
  setLabelStack = const
  overLabelStack = id

-- | Pushes a label onto a label stack in the scope of a parser.
localPushLabel :: (HasLabelStack l r, MonadReader r m) => l -> m a -> m a
localPushLabel = local . overLabelStack . pushLabel

-- | Reads a 'LabelStack'.
askLabelStack :: (HasLabelStack l r, MonadReader r m) => m (LabelStack l)
askLabelStack = asks viewLabelStack

data LabelledError l e = LabelledError
  { leLabels :: !(LabelStack l)
  , leError :: !e
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasLabelStack l (LabelledError l e) where
  viewLabelStack = leLabels
  setLabelStack ls le = le { leLabels = ls }

labelledError :: Foldable f => f l -> e -> LabelledError l e
labelledError = LabelledError . LabelStack . Seq.fromList . toList

unlabelledError :: e -> LabelledError l e
unlabelledError = LabelledError emptyLabelStack
