module SimpleParser.Examples.Common.Sexp
  ( Atom (..)
  , SexpF (..)
  , Sexp (..)
  )
where

import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import SimpleParser.Explain (ShowTextBuildable (..), TextBuildable)

data Atom
  = AtomIdent !Text
  | AtomString !Text
  | AtomInt !Integer
  | AtomSci !Scientific
  deriving stock (Eq, Show)
  deriving (TextBuildable) via (ShowTextBuildable Atom)

data SexpF a
  = SexpAtom !Atom
  | SexpList !(Seq a)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

newtype Sexp = Sexp {unSexp :: SexpF Sexp}
  deriving stock (Eq, Show)
