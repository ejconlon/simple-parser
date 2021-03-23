module SimpleParser.Examples.Sexp
  ( Sexp (..)
  , SexpF (..)
  , Atom (..)
  , sexpParser
  , rootSexpParser
  ) where

import Control.Monad (void)
import Data.Char (isDigit, isSpace)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import SimpleParser.Common (betweenParser, decimalParser, escapedStringParser, lexemeParser, scientificParser,
                            sepByParser, signedParser, spaceParser)
import SimpleParser.Input (matchToken, satisfyToken, takeTokensWhile)
import SimpleParser.Parser (ParserT, branchParser, isolateParser)
import SimpleParser.Stream (Chunked (..), TextualStream, packChunk)

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

sexpParser :: (TextualStream s, Monad m) => ParserT e s m Sexp
sexpParser = let p = fmap Sexp (rootSexpParser p) in p

rootSexpParser :: (TextualStream s, Monad m) => ParserT e s m a -> ParserT e s m (SexpF a)
rootSexpParser root = isolateParser $ branchParser
  [ fmap SexpList (listP root)
  , fmap SexpAtom atomP
  ]

nonDelimPred :: Char -> Bool
nonDelimPred c = c /= '(' && c /= ')' && not (isSpace c)

identStartPred :: Char -> Bool
identStartPred c = not (isDigit c) && identContPred c

identContPred :: Char -> Bool
identContPred c = c /= '"' && nonDelimPred c

stringP :: (TextualStream s, Monad m) => ParserT e s m Text
stringP = fmap packChunk (escapedStringParser '"')

identifierP :: (TextualStream s, Monad m) => ParserT e s m Text
identifierP = do
  x <- satisfyToken identStartPred
  xs <- takeTokensWhile identContPred
  pure (packChunk (consChunk x xs))

spaceP :: (TextualStream s, Monad m) => ParserT e s m ()
spaceP = spaceParser

lexP :: (TextualStream s, Monad m) => ParserT e s m a -> ParserT e s m a
lexP = lexemeParser spaceP

openParenP :: (TextualStream s, Monad m) => ParserT e s m ()
openParenP = lexP (void (matchToken '('))

closeParenP :: (TextualStream s, Monad m) => ParserT e s m ()
closeParenP = lexP (void (matchToken ')'))

intP :: (TextualStream s, Monad m) => ParserT e s m Integer
intP = signedParser (pure ()) decimalParser

floatP :: (TextualStream s, Monad m) => ParserT e s m Scientific
floatP = signedParser (pure ()) scientificParser

atomP :: (TextualStream s, Monad m) => ParserT e s m Atom
atomP = lexP $ isolateParser $ branchParser
  [ fmap AtomString stringP
  , fmap AtomInt intP
  , fmap AtomFloat floatP
  , fmap AtomIdent identifierP
  ]

listP :: (TextualStream s, Monad m) => ParserT e s m a -> ParserT e s m (Seq a)
listP root = lexP (fmap Seq.fromList (betweenParser openParenP closeParenP (sepByParser root spaceP)))
