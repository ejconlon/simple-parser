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

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Char (isDigit, isSpace)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import SimpleParser (Chunked (..), EmbedTextLabel (..), ExplainLabel (..), MatchBlock (..), MatchCase (..), Parser,
                     TextLabel, TextualStream, anyToken, applySign, betweenParser, escapedStringParser, lexemeParser,
                     lookAheadMatch, matchToken, numParser, packChunk, popChunk, satisfyToken, sepByParser, signParser,
                     signedNumStartPred, spaceParser, takeTokensWhile)

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

data SexpLabel =
    SexpLabelIdentStart
  | SexpLabelEmbedText !TextLabel
  | SexpLabelCustom !Text
  deriving (Eq, Show)

instance ExplainLabel SexpLabel where
  explainLabel sl =
    case sl of
      SexpLabelIdentStart -> "start of identifier"
      SexpLabelEmbedText tl -> explainLabel tl
      _ -> undefined

instance EmbedTextLabel SexpLabel where
  embedTextLabel = SexpLabelEmbedText

newtype Sexp = Sexp { unSexp :: SexpF Sexp }
  deriving (Eq, Show)

type SexpParserC s = TextualStream s

type SexpParserM s a = Parser SexpLabel s Void a

sexpParser :: SexpParserC s => SexpParserM s Sexp
sexpParser = let p = fmap Sexp (recSexpParser p) in p

recSexpParser :: SexpParserC s => SexpParserM s a -> SexpParserM s (SexpF a)
recSexpParser root = lookAheadMatch block where
  block = MatchBlock anyToken (fmap SexpAtom atomP)
    [ MatchCase Nothing (== '(') (fmap SexpList (listP root))
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

numAtomP :: SexpParserC s => SexpParserM s Atom
numAtomP = do
  ms <- signParser
  n <- numParser
  case n of
    Left i -> pure (AtomInt (applySign ms i))
    Right s -> pure (AtomSci (applySign ms s))

chunk1 :: SexpParserC s => SexpParserM s Text
chunk1 = do
  mc <- popChunk 2
  case mc of
    Just c | not (chunkEmpty c) -> pure (packChunk c)
    _ -> empty

unaryIdentPred :: Char -> Text -> Bool
unaryIdentPred u t0 =
  case T.uncons t0 of
    Just (c0, t1) | u == c0 ->
      case T.uncons t1 of
        Just (c1, _) -> not (isDigit c1)
        Nothing -> True
    _ -> False

identAtomP :: SexpParserC s => SexpParserM s Atom
identAtomP = fmap AtomIdent identifierP

atomP :: SexpParserC s => SexpParserM s Atom
atomP = lexP (lookAheadMatch block) where
  block = MatchBlock chunk1 (fail "failed to parse sexp atom")
    [ MatchCase Nothing ((== '"') . T.head) (fmap AtomString stringP)
    , MatchCase Nothing (unaryIdentPred '+') identAtomP
    , MatchCase Nothing (unaryIdentPred '-') identAtomP
    , MatchCase Nothing (signedNumStartPred . T.head) numAtomP
    , MatchCase Nothing (identStartPred . T.head) identAtomP
    ]

listP :: SexpParserC s => SexpParserM s a -> SexpParserM s (Seq a)
listP root = lexP (betweenParser openParenP closeParenP (sepByParser root spaceP))
