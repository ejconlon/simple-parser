{-# LANGUAGE GADTs #-}

module SimpleParser.Examples.Ast where

import Data.Text (Text)
import SimpleParser (TextualStream, Parser)
import SimpleParser.Examples.Sexp (SexpLabel, SexpParserC, SexpParserM)
import Data.Void (Void)
import Data.Sequence (Seq)
import Data.Map.Strict (Map)

-- Parses sexp-formatted ASTs

data Ctor s t where
  Ctor0 :: t -> Ctor s t
  Ctor1 :: (a -> Either String t) -> SexpParserM s a -> Ctor s t
  Ctor2 :: (a -> b -> Either String t) -> SexpParserM s a -> SexpParserM s b -> Ctor s t
  Ctor3 :: (a -> b -> c -> Either String t) -> SexpParserM s a -> SexpParserM s b -> SexpParserM s c -> Ctor s t
  Ctor4 :: (a -> b -> c -> d -> Either String t) -> SexpParserM s a -> SexpParserM s b -> SexpParserM s c -> SexpParserM s d -> Ctor s t
  Ctor5 :: (a -> b -> c -> d -> e -> Either String t) -> SexpParserM s a -> SexpParserM s b -> SexpParserM s c -> SexpParserM s d -> SexpParserM s e -> Ctor s t
  CtorN :: (Seq a -> Either String t) -> SexpParserM s a -> Ctor s t

data Defn s t = Defn
  { defnLists :: Map Text (Ctor s t)
  }

astParser :: SexpParserC s => (SexpParserM s t -> Defn s t) -> SexpParserM s t
astParser mkCtors = let p = recAstParser (mkCtors p) in p

recSexpParser :: SexpParserC s => Defn s t -> SexpParserM s t
recSexpParser root = lookAheadMatch block where
  block = MatchBlock anyToken (fmap SexpAtom atomP)
    [ MatchCase Nothing (== '(') (fmap SexpList (listP root))
    ]
