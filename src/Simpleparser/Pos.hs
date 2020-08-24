module SimpleParser.Pos where

import Lens.Micro (Lens')
import Prelude

data Pos = Pos { _posOffset :: !Int, _posLine :: !Int, _posColumn :: !Int } deriving (Eq, Show)

emptyPos :: Pos
emptyPos = Pos 0 0 0

class HasPos s where
  posL :: Lens' s Pos

instance HasPos Pos where
  posL = id
