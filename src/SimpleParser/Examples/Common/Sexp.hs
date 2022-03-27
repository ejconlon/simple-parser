module SimpleParser.Examples.Common.Sexp
  ( Atom (..)
  , SexpF (..)
  , Sexp (..)
  ) where

import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)

data Atom =
    AtomIdent !Text
  | AtomString !Text
  | AtomInt !Integer
  | AtomSci !Scientific
  deriving (Eq, Show)

data SexpF a =
    SexpAtom !Atom
  | SexpList !(Seq a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Sexp = Sexp { unSexp :: SexpF Sexp }
  deriving (Eq, Show)
