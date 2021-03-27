{-# LANGUAGE OverloadedStrings #-}

module SimpleParser.Examples.Sexp
  ( Sexp (..)
  , SexpF (..)
  , Atom (..)
  , SexpParser
  , sexpParser
  , recSexpParser
  ) where

import Control.Monad (void)
import Data.Char (isDigit, isSpace)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import SimpleParser.Chunked (Chunked (..), packChunk)
import SimpleParser.Common (betweenParser, decimalParser, escapedStringParser, exclusiveParser, lexemeParser,
                            scientificParser, sepByParser, signedParser, spaceParser)
import SimpleParser.Input (matchToken, satisfyToken, takeTokensWhile)
import SimpleParser.Labels (CompoundLabel (..))
import SimpleParser.Parser (ParserT)
import SimpleParser.Stream (TextualStream)

data Atom =
    AtomIdent !Text
  | AtomString !Text
  | AtomInt !Integer
  | AtomFloat !Scientific
  deriving (Eq, Show)

data SexpF a =
    SexpAtom !Atom
  | SexpList !(Seq a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data SexpLabel =
    SexpLabelBranch !Text
  | SexpLabelIdentStart
  deriving (Eq, Show)

newtype Sexp = Sexp { unSexp :: SexpF Sexp }
  deriving (Eq, Show)

type SexpParser l s m = (l ~ SexpLabel, TextualStream s, Monad m)

sexpParser :: SexpParser l s m => ParserT l s e m Sexp
sexpParser = let p = fmap Sexp (recSexpParser p) in p

recSexpParser :: SexpParser l s m => ParserT l s e m a -> ParserT l s e m (SexpF a)
recSexpParser root = exclusiveParser
  [ (SexpLabelBranch "list", fmap SexpList (listP root))
  , (SexpLabelBranch "atom", fmap SexpAtom atomP)
  ]

nonDelimPred :: Char -> Bool
nonDelimPred c = c /= '(' && c /= ')' && not (isSpace c)

identStartPred :: Char -> Bool
identStartPred c = not (isDigit c) && identContPred c

identContPred :: Char -> Bool
identContPred c = c /= '"' && nonDelimPred c

stringP :: SexpParser l s m => ParserT l s e m Text
stringP = fmap packChunk (escapedStringParser '"')

identifierP :: SexpParser l s m => ParserT l s e m Text
identifierP = do
  x <- satisfyToken (Just (CompoundLabelCustom SexpLabelIdentStart)) identStartPred
  xs <- takeTokensWhile identContPred
  pure (packChunk (consChunk x xs))

spaceP :: SexpParser l s m => ParserT l s e m ()
spaceP = spaceParser

lexP :: SexpParser l s m => ParserT l s e m a -> ParserT l s e m a
lexP = lexemeParser spaceP

openParenP :: SexpParser l s m => ParserT l s e m ()
openParenP = lexP (void (matchToken '('))

closeParenP :: SexpParser l s m => ParserT l s e m ()
closeParenP = lexP (void (matchToken ')'))

intP :: SexpParser l s m => ParserT l s e m Integer
intP = signedParser (pure ()) decimalParser

floatP :: SexpParser l s m => ParserT l s e m Scientific
floatP = signedParser (pure ()) scientificParser

atomP :: SexpParser l s m => ParserT l s e m Atom
atomP = lexP $ exclusiveParser
  [ (SexpLabelBranch "string", fmap AtomString stringP)
  , (SexpLabelBranch "int", fmap AtomInt intP)
  , (SexpLabelBranch "float", fmap AtomFloat floatP)
  , (SexpLabelBranch "identifier", fmap AtomIdent identifierP)
  ]

listP :: SexpParser l s m => ParserT l s e m a -> ParserT l s e m (Seq a)
listP root = lexP (betweenParser openParenP closeParenP (sepByParser root spaceP))
