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
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Void (Void)
import SimpleParser (Chunked (..), EmbedTextLabel (..), ExplainLabel (..), MatchBlock (..), MatchCase (..), Parser,
                     TextLabel, TextualStream, anyToken, betweenParser, commitParser, escapedStringParser, lexemeParser,
                     lookAheadMatch, matchToken, onEmptyParser, orParser, packChunk, satisfyToken, scientificParser,
                     sepByParser, signedNumStartPred, signedParser, spaceParser, takeTokensWhile)

data Atom =
    AtomIdent !Text
  | AtomString !Text
  | AtomInt !Int
  | AtomFloat !Scientific
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
recSexpParser root = onEmptyParser (orParser lp ap) (fail "failed to parse sexp document") where
  lp = commitParser openParenP (fmap SexpList (listP root))
  ap = fmap SexpAtom atomP

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

floatP :: SexpParserC s => SexpParserM s Scientific
floatP = signedParser (pure ()) scientificParser

-- Since intP is a subset of floatP, we ignore errors there and let floatP report them.
numP :: SexpParserC s => SexpParserM s Atom
numP = do
  s <- floatP
  case toBoundedInteger s of
    Just i -> pure (AtomInt i)
    Nothing -> pure (AtomFloat s)

atomP :: SexpParserC s => SexpParserM s Atom
atomP = lexP (lookAheadMatch block) where
  -- TODO allow `+` and `-` identifiers - need to take a 2-chunk for that
  block = MatchBlock anyToken (fail "failed to parse sexp atom")
    [ MatchCase Nothing (== '"') (fmap AtomString stringP)
    , MatchCase Nothing signedNumStartPred numP
    , MatchCase Nothing identStartPred (fmap AtomIdent identifierP)
    ]

listP :: SexpParserC s => SexpParserM s a -> SexpParserM s (Seq a)
listP root = lexP (betweenParser openParenP closeParenP (sepByParser root spaceP))
