module SimpleParser.Examples.Sexp
  ( Sexp (..)
  , SexpF (..)
  , Atom (..)
  , SexpParser
  , sexpParser
  , rootSexpParser
  ) where

import Control.Monad (void)
import Data.Char (isDigit, isSpace)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import SimpleParser.Chunked (Chunked (..), packChunk)
import SimpleParser.Common (betweenParser, decimalParser, escapedStringParser, lexemeParser, scientificParser,
                            sepByParser, signedParser, spaceParser)
import SimpleParser.Input (matchToken, satisfyToken, takeTokensWhile)
import SimpleParser.Parser (ParserT, andAllParser, isolateParser)
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

newtype Sexp = Sexp { unSexp :: SexpF Sexp }
  deriving (Eq, Show)

type SexpParser s m = (TextualStream s, Monad m)

sexpParser :: SexpParser s m => ParserT e s m Sexp
sexpParser = let p = fmap Sexp (rootSexpParser p) in p

rootSexpParser :: SexpParser s m => ParserT e s m a -> ParserT e s m (SexpF a)
rootSexpParser root = isolateParser $ andAllParser
  [ fmap SexpList (listP root)
  , fmap SexpAtom atomP
  ]

nonDelimPred :: Char -> Bool
nonDelimPred c = c /= '(' && c /= ')' && not (isSpace c)

identStartPred :: Char -> Bool
identStartPred c = not (isDigit c) && identContPred c

identContPred :: Char -> Bool
identContPred c = c /= '"' && nonDelimPred c

stringP :: SexpParser s m => ParserT e s m Text
stringP = fmap packChunk (escapedStringParser '"')

identifierP :: SexpParser s m => ParserT e s m Text
identifierP = do
  x <- satisfyToken identStartPred
  xs <- takeTokensWhile identContPred
  pure (packChunk (consChunk x xs))

spaceP :: SexpParser s m => ParserT e s m ()
spaceP = spaceParser

lexP :: SexpParser s m => ParserT e s m a -> ParserT e s m a
lexP = lexemeParser spaceP

openParenP :: SexpParser s m => ParserT e s m ()
openParenP = lexP (void (matchToken '('))

closeParenP :: SexpParser s m => ParserT e s m ()
closeParenP = lexP (void (matchToken ')'))

intP :: SexpParser s m => ParserT e s m Integer
intP = signedParser (pure ()) decimalParser

floatP :: SexpParser s m => ParserT e s m Scientific
floatP = signedParser (pure ()) scientificParser

atomP :: SexpParser s m => ParserT e s m Atom
atomP = lexP $ isolateParser $ andAllParser
  [ fmap AtomString stringP
  , fmap AtomInt intP
  , fmap AtomFloat floatP
  , fmap AtomIdent identifierP
  ]

listP :: SexpParser s m => ParserT e s m a -> ParserT e s m (Seq a)
listP root = lexP (betweenParser openParenP closeParenP (sepByParser root spaceP))
