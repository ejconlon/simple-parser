{-# LANGUAGE OverloadedStrings #-}

-- | Parses S-expressions but lexes first
module SimpleParser.Examples.Lexed.Sexp
  ( Sexp (..)
  , SexpF (..)
  , Atom (..)
  , SexpTokLabel (..)
  , SexpTokParserC
  , runSexpParser
  ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Char (isDigit, isSpace)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import SimpleParser (Chunked (..), EmbedTextLabel (..), ExplainLabel (..), MatchBlock (..), MatchCase (..), Parser,
                     TextLabel, TextualStream, anyToken, applySign, betweenParser, escapedStringParser, lexemeParser,
                     lookAheadMatch, matchToken, numParser, packChunk, popChunk, satisfyToken, signParser,
                     signedNumStartPred, spaceParser, takeTokensWhile, Stream (..), popToken, greedyStarParser, runParserLexed, PosStream (..))
import SimpleParser.Examples.Common.Sexp (Atom (..), Sexp (..), SexpF(..))
import Control.Monad.Catch (MonadThrow)
import Data.Typeable (Typeable)

-- First, our tokenizer:

data SexpTokLabel =
    SexpTokLabelIdentStart
  | SexpTokLabelEmbedText !TextLabel
  deriving (Eq, Show)

instance ExplainLabel SexpTokLabel where
  explainLabel sl =
    case sl of
      SexpTokLabelIdentStart -> "start of identifier"
      SexpTokLabelEmbedText tl -> explainLabel tl

instance EmbedTextLabel SexpTokLabel where
  embedTextLabel = SexpTokLabelEmbedText

type SexpTokParserC s = (PosStream s, TextualStream s)

type SexpTokParserM s a = Parser SexpTokLabel s Void a

data SexpTok =
    SexpTokOpenParen
  | SexpTokCloseParen
  | SexpTokAtom !Atom
  deriving stock (Eq, Show)

nonDelimPred :: Char -> Bool
nonDelimPred c = c /= '(' && c /= ')' && not (isSpace c)

identStartPred :: Char -> Bool
identStartPred c = not (isDigit c) && identContPred c

identContPred :: Char -> Bool
identContPred c = c /= '"' && nonDelimPred c

stringTP :: SexpTokParserC s => SexpTokParserM s Text
stringTP = fmap packChunk (escapedStringParser '"')

identifierTP :: SexpTokParserC s => SexpTokParserM s Text
identifierTP = do
  x <- satisfyToken (Just SexpTokLabelIdentStart) identStartPred
  xs <- takeTokensWhile identContPred
  pure (packChunk (consChunk x xs))

spaceTP :: SexpTokParserC s => SexpTokParserM s ()
spaceTP = spaceParser

lexTP :: SexpTokParserC s => SexpTokParserM s a -> SexpTokParserM s a
lexTP = lexemeParser spaceTP

openParenTP :: SexpTokParserC s => SexpTokParserM s ()
openParenTP = lexTP (void (matchToken '('))

closeParenTP :: SexpTokParserC s => SexpTokParserM s ()
closeParenTP = lexTP (void (matchToken ')'))

numAtomTP :: SexpTokParserC s => SexpTokParserM s Atom
numAtomTP = do
  ms <- signParser
  n <- numParser
  case n of
    Left i -> pure (AtomInt (applySign ms i))
    Right s -> pure (AtomSci (applySign ms s))

chunk1 :: SexpTokParserC s => SexpTokParserM s Text
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

identAtomTP :: SexpTokParserC s => SexpTokParserM s Atom
identAtomTP = fmap AtomIdent identifierTP

atomTP :: SexpTokParserC s => SexpTokParserM s Atom
atomTP = lookAheadMatch block where
  block = MatchBlock chunk1 (fail "failed to parse sexp atom")
    [ MatchCase Nothing ((== '"') . T.head) (fmap AtomString stringTP)
    , MatchCase Nothing (unaryIdentPred '+') identAtomTP
    , MatchCase Nothing (unaryIdentPred '-') identAtomTP
    , MatchCase Nothing (signedNumStartPred . T.head) numAtomTP
    , MatchCase Nothing (identStartPred . T.head) identAtomTP
    ]

sexpTokParser :: SexpTokParserC s => SexpTokParserM s SexpTok
sexpTokParser= lookAheadMatch block where
  block = MatchBlock anyToken (fmap SexpTokAtom atomTP)
    [ MatchCase Nothing (== '(') (SexpTokOpenParen <$ openParenTP)
    , MatchCase Nothing (== ')') (SexpTokCloseParen <$ closeParenTP)
    ]

-- Now the Sexp parser itself:

type SexpParserC s = (Stream s, Token s ~ SexpTok)

type SexpParserM s a = Parser Void s Void a

isOpenParenTok, isCloseParenTok, isAtomTok :: SexpTok -> Bool
isOpenParenTok = \case { SexpTokOpenParen -> True; _ -> False }
isCloseParenTok = \case { SexpTokCloseParen -> True; _ -> False }
isAtomTok = \case { SexpTokAtom _ -> True; _ -> False }

atomP :: SexpParserC s => SexpParserM s Atom
atomP = popToken >>= \case { Just (SexpTokAtom a) -> pure a; _ -> empty }

openParenP, closeParenP :: SexpParserC s => SexpParserM s ()
openParenP = popToken >>= \case { Just SexpTokOpenParen -> pure (); _ -> empty }
closeParenP = popToken >>= \case { Just SexpTokCloseParen -> pure (); _ -> empty }

listP :: SexpParserC s => SexpParserM s a -> SexpParserM s (Seq a)
listP root = betweenParser openParenP closeParenP (greedyStarParser root)

recSexpParser :: SexpParserC s => SexpParserM s a -> SexpParserM s (SexpF a)
recSexpParser root = lookAheadMatch block where
  block = MatchBlock anyToken (error "impossible")
    [ MatchCase Nothing isOpenParenTok (fmap SexpList (listP root))
    , MatchCase Nothing isCloseParenTok (fail "invalid close paren")
    , MatchCase Nothing isAtomTok (fmap SexpAtom atomP)
    ]

sexpParser :: SexpParserC s => SexpParserM s Sexp
sexpParser = let p = fmap Sexp (recSexpParser p) in p

-- And combined:

runSexpParser :: (
  Typeable s, Typeable (Token s), Typeable (Chunk s), Typeable (Pos s),
  Show s, Show (Token s), Show (Chunk s), Show (Pos s),
  SexpTokParserC s, MonadThrow m) => s -> m Sexp
runSexpParser = runParserLexed sexpTokParser sexpParser
