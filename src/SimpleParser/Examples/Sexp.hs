{-# LANGUAGE OverloadedStrings #-}

module SimpleParser.Examples.Sexp
  ( Sexp (..)
  , SexpF (..)
  , Atom (..)
  , SexpLabel (..)
  , SexpParserC
  , SexpParserM
  , sexpParser
  , recSexpParser
  ) where

import Control.Monad (void)
import Data.Char (isDigit, isSpace)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Void (Void)
import SimpleParser.Chunked (Chunked (..), packChunk)
import SimpleParser.Common (EmbedTextLabel (..), TextLabel, betweenParser, decimalParser, escapedStringParser,
                            exclusiveParser, lexemeParser, scientificParser, sepByParser, signedParser, spaceParser)
import SimpleParser.Explain (ExplainLabel (..))
import SimpleParser.Input (matchToken, satisfyToken, takeTokensWhile)
import SimpleParser.Parser (Parser, markParser)
import SimpleParser.Stream (TextualStream)
import qualified Text.Builder as TB

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
  | SexpLabelEmbedText !TextLabel
  deriving (Eq, Show)

instance ExplainLabel SexpLabel where
  explainLabel sl =
    case sl of
      SexpLabelBranch b -> "branch " <> TB.text b
      SexpLabelIdentStart -> "start of identifier"
      SexpLabelEmbedText tl -> explainLabel tl

instance EmbedTextLabel SexpLabel where
  embedTextLabel = SexpLabelEmbedText

newtype Sexp = Sexp { unSexp :: SexpF Sexp }
  deriving (Eq, Show)

type SexpParserC s = TextualStream s

type SexpParserM s a = Parser SexpLabel s Void a

sexpParser :: SexpParserC s => SexpParserM s Sexp
sexpParser = let p = fmap Sexp (recSexpParser p) in p

recSexpParser :: SexpParserC s => SexpParserM s a -> SexpParserM s (SexpF a)
recSexpParser root = markParser $ exclusiveParser
  [ (SexpLabelBranch "list", fmap SexpList (listP root))
  , (SexpLabelBranch "atom", fmap SexpAtom atomP)
  ]

nonDelimPred :: Char -> Bool
nonDelimPred c = c /= '(' && c /= ')' && not (isSpace c)

identStartPred :: Char -> Bool
identStartPred c = not (isDigit c) && identContPred c

identContPred :: Char -> Bool
identContPred c = c /= '"' && nonDelimPred c

stringP :: SexpParserC s => SexpParserM s Text
stringP = fmap packChunk (escapedStringParser '"')

identifierP :: SexpParserC s => SexpParserM s Text
identifierP = do
  x <- satisfyToken (Just SexpLabelIdentStart) identStartPred
  xs <- takeTokensWhile identContPred
  pure (packChunk (consChunk x xs))

spaceP :: SexpParserC s => SexpParserM s ()
spaceP = spaceParser

lexP :: SexpParserC s => SexpParserM s a -> SexpParserM s a
lexP = lexemeParser spaceP

openParenP :: SexpParserC s => SexpParserM s ()
openParenP = lexP (void (matchToken '('))

closeParenP :: SexpParserC s => SexpParserM s ()
closeParenP = lexP (void (matchToken ')'))

intP :: SexpParserC s => SexpParserM s Integer
intP = signedParser (pure ()) decimalParser

floatP :: SexpParserC s => SexpParserM s Scientific
floatP = signedParser (pure ()) scientificParser

atomP :: SexpParserC s => SexpParserM s Atom
atomP = lexP $ markParser $ exclusiveParser
  [ (SexpLabelBranch "string", fmap AtomString stringP)
  , (SexpLabelBranch "int", fmap AtomInt intP)
  , (SexpLabelBranch "float", fmap AtomFloat floatP)
  , (SexpLabelBranch "identifier", fmap AtomIdent identifierP)
  ]

listP :: SexpParserC s => SexpParserM s a -> SexpParserM s (Seq a)
listP root = lexP (betweenParser openParenP closeParenP (sepByParser root spaceP))
