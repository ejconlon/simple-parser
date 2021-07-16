module SimpleParser.Stack
  ( Stack (..)
  , emptyStack
  , pushStack
  , topStack
  , bottomStack
  , bottomUpStack
  ) where

import Data.Sequence (Seq (..))

-- | A stack supporting O(1) push, top, and bottom.
-- Behind the newtype, a "push" onto the stack is implemented as "snoc", therefore
-- fold/traverse goes from bottom of stack (most generic label) to top (most specific label).
newtype Stack a = Stack
  { unStack :: Seq a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Easy constructor for the empty stack
emptyStack :: Stack a
emptyStack = Stack Empty

-- | Pushes a an element onto a 'Stack'
pushStack :: a -> Stack a -> Stack a
pushStack a = Stack . (:|> a) . unStack

-- | Returns the top element of the stack (most recently pushed).
topStack :: Stack a -> Maybe a
topStack (Stack s) =
  case s of
    Empty -> Nothing
    _ :|> a -> Just a

-- | Returns the bottom element of the stack (least recently pushed).
bottomStack :: Stack a -> Maybe a
bottomStack (Stack s) =
  case s of
    Empty -> Nothing
    a :<| _ -> Just a

-- | Selects elements from the bottom of the stack to the top.
bottomUpStack :: (a -> Maybe b) -> Stack a -> Seq b
bottomUpStack f = go Empty . unStack where
  go !acc s =
    case s of
      Empty -> acc
      a :<| s' ->
        let acc' = maybe acc (acc :|>) (f a)
        in go acc' s'
