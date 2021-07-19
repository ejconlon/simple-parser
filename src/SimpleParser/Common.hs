-- | Common parsers.
-- See <https://hackage.haskell.org/package/megaparsec-9.0.1/docs/Text-Megaparsec-Char-Lexer.html Text.Megaparsec.Char.Lexer>.
module SimpleParser.Common
  ( TextLabel (..)
  , EmbedTextLabel (..)
  , CompoundTextLabel (..)
  , sepByParser
  , betweenParser
  , lexemeParser
  , newlineParser
  , spaceParser
  , hspaceParser
  , spaceParser1
  , hspaceParser1
  , decimalParser
  , signedNumStartPred
  , scientificParser
  , numParser
  , Sign (..)
  , signParser
  , applySign
  , signedParser
  , escapedStringParser
  , spanParser
  , getStreamPos
  ) where

import Control.Monad (void)
import Control.Monad.State (get, gets)
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import SimpleParser.Chunked (Chunked (..))
import SimpleParser.Input (dropTokensWhile, dropTokensWhile1, foldTokensWhile, matchToken, peekToken, popToken,
                           takeTokensWhile1)
import SimpleParser.Parser (ParserT, defaultParser, greedyStarParser, optionalParser, orParser)
import SimpleParser.Stream (PosStream (..), Span (..), Stream (..))

-- | Enumeration of common labels in textual parsing.
data TextLabel =
    TextLabelSpace
  | TextLabelHSpace
  | TextLabelDigit
  deriving (Eq, Show)

class EmbedTextLabel l where
  embedTextLabel :: TextLabel -> l

instance EmbedTextLabel TextLabel where
  embedTextLabel = id

-- | Union of text and custom labels
data CompoundTextLabel l =
    CompoundTextLabelText !TextLabel
  | CompoundTextLabelCustom !l
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance EmbedTextLabel (CompoundTextLabel l) where
  embedTextLabel = CompoundTextLabelText

-- | Yields the maximal list of separated items. May return an empty list.
sepByParser :: (Chunked seq elem, Monad m) =>
  -- | How to parse item
  ParserT l s e m elem ->
  -- | How to parse separator
  ParserT l s e m () ->
  ParserT l s e m seq
sepByParser thing sep = do
  ma <- optionalParser thing
  case ma of
    Nothing -> pure mempty
    Just a -> do
      as <- greedyStarParser (sep *> thing)
      pure (consChunk a as)

-- | Parses between start and end markers.
betweenParser :: Monad m =>
  -- | How to parse start
  ParserT l s e m () ->
  -- | How to parse end
  ParserT l s e m () ->
  -- | How to parse inside
  ParserT l s e m a ->
  ParserT l s e m a
betweenParser start end thing = do
  start
  a <- thing
  end
  pure a

-- | A wrapper for lexemes (equivalent to Megaparsec's 'lexeme').
lexemeParser :: Monad m =>
  -- | How to consume white space after lexeme
  ParserT l s e m () ->
  -- | How to parse actual lexeme
  ParserT l s e m a ->
  ParserT l s e m a
lexemeParser spc p = p <* spc

-- | Consumes a newline character.
newlineParser :: (Stream s, Token s ~ Char, Monad m) => ParserT l s e m ()
newlineParser = void (matchToken '\n')

-- | Consumes 0 or more space characters.
spaceParser :: (Stream s, Token s ~ Char, Monad m) => ParserT l s e m ()
spaceParser = void (dropTokensWhile isSpace)

isHSpace :: Char -> Bool
isHSpace c = isSpace c && c /= '\n' && c /= '\r'

-- | Consumes 0 or more non-line-break space characters
hspaceParser :: (Stream s, Token s ~ Char, Monad m) => ParserT l s e m ()
hspaceParser = void (dropTokensWhile isHSpace)

-- | Consumes 1 or more space characters.
spaceParser1 :: (EmbedTextLabel l, Stream s, Token s ~ Char, Monad m) => ParserT l s e m ()
spaceParser1 = void (dropTokensWhile1 (Just (embedTextLabel TextLabelSpace)) isSpace)

-- | Consumes 1 or more non-line-break space characters
hspaceParser1 :: (EmbedTextLabel l, Stream s, Token s ~ Char, Monad m) => ParserT l s e m ()
hspaceParser1 = void (dropTokensWhile1 (Just (embedTextLabel TextLabelHSpace)) isHSpace)

-- | Parses an integer in decimal representation (equivalent to Megaparsec's 'decimal').
decimalParser :: (EmbedTextLabel l, Stream s, Token s ~ Char, Monad m, Num a) => ParserT l s e m a
decimalParser = fmap mkNum (takeTokensWhile1 (Just (embedTextLabel TextLabelDigit)) isDigit) where
  mkNum = foldl' step 0 . chunkToTokens
  step a c = a * 10 + fromIntegral (digitToInt c)

data SP = SP !Integer !Int

dotDecimalParser :: (EmbedTextLabel l, Stream s, Token s ~ Char, Monad m) => Integer -> ParserT l s e m SP
dotDecimalParser c' = do
  void (matchToken '.')
  let mkNum = foldl' step (SP c' 0) . chunkToTokens
      step (SP a e') c = SP (a * 10 + fromIntegral (digitToInt c)) (e' - 1)
  fmap mkNum (takeTokensWhile1 (Just (embedTextLabel TextLabelDigit)) isDigit)

exponentParser :: (EmbedTextLabel l, Stream s, Token s ~ Char, Monad m) => Int -> ParserT l s e m Int
exponentParser e' = do
  void (orParser (matchToken 'e') (matchToken 'E'))
  fmap (+ e') (signedParser (pure ()) decimalParser)

-- | Predicate for satisfying the start of signed numbers
signedNumStartPred :: Char -> Bool
signedNumStartPred c = isDigit c || c == '+' || c == '-'

-- | Parses a floating point value as a 'Scientific' number (equivalent to Megaparsec's 'scientific').
scientificParser :: (EmbedTextLabel l, Stream s, Token s ~ Char, Monad m) => ParserT l s e m Scientific
scientificParser = do
  c' <- decimalParser
  SP c e' <- defaultParser (SP c' 0) (dotDecimalParser c')
  e <- defaultParser e' (exponentParser e')
  pure (Sci.scientific c e)

-- | Parses a number as a literal integer or a 'Scientific' number.
-- Though 'Scientific' can represent integers, this allows you to distinugish integer literals from scientific literals
-- since that information is lost after parsing.
numParser :: (EmbedTextLabel l, Stream s, Token s ~ Char, Monad m) => ParserT l s e m (Either Integer Scientific)
numParser = do
  c' <- decimalParser
  (SP c e', b1) <- defaultParser (SP c' 0, False) (fmap (,True) (dotDecimalParser c'))
  (e, b2) <- defaultParser (e', False) (fmap (,True) (exponentParser e'))
  -- If there is no decimal or exponent, return this as an integer
  -- Otherwise return as scientific, which may be float or exponentiated integer
  if not b1 && not b2
    then pure (Left c')
    else pure (Right (Sci.scientific c e))

data Sign = SignPos | SignNeg deriving (Eq, Show)

-- | Consumes an optional + or - representing the sign of a number.
signParser :: (Stream s, Token s ~ Char, Monad m) => ParserT l s e m (Maybe Sign)
signParser = do
  mc <- peekToken
  case mc of
    Just '+' -> popToken $> Just SignPos
    Just '-' -> popToken $> Just SignNeg
    _ -> pure Nothing

-- | Optionally negate the number according to the sign (treating 'Nothing' as positive sign).
applySign :: Num a => Maybe Sign -> a -> a
applySign ms n =
  case ms of
    Just SignNeg -> negate n
    _ -> n

-- | Parses an optional sign character followed by a number and yields a correctly-signed
-- number (equivalend to Megaparsec's 'signed').
signedParser :: (Stream s, Token s ~ Char, Monad m, Num a) =>
  -- | How to consume white space after the sign
  ParserT l s e m () ->
  -- | How to parse the number itself
  ParserT l s e m a ->
  -- | Parser for signed numbers
  ParserT l s e m a
signedParser spc p = do
  ms <- signParser
  spc
  fmap (applySign ms) p

data Pair = Pair ![Char] !Bool

-- | Given a quote charcter (like a single or double quote), yields the contents of the
-- string bounded by those quotes. The contents may contain backslash-escaped quotes.
-- Returns nothing if outside quotes are missing or the stream ends before unquote.
escapedStringParser :: (Stream s, Token s ~ Char, Monad m) => Char -> ParserT l s e m (Chunk s)
escapedStringParser quoteChar =
  let quoteParser = void (matchToken quoteChar)
      accParser = foldTokensWhile go (Pair [] False)
      innerParser = fmap (\(Pair acc _) -> revTokensToChunk acc) accParser
      escChar = '\\'
      go c (Pair acc esc)
        | c == escChar =
          if esc
            then (True, Pair (escChar:acc) False) -- Was escaped escape, append one
            else (True, Pair acc True) -- Skip appending this esc
        | c == quoteChar =
          if esc
            then (True, Pair (c:acc) False) -- Escaped quote
            else (False, Pair acc False) -- End of quote
        | otherwise =
          if esc
            then (True, Pair (c:escChar:acc) False) -- Was a non-quote esc, append both
            else (True, Pair (c:acc) False) -- Just consume char
  in betweenParser quoteParser quoteParser innerParser

-- | Adds span information to parsed values.
spanParser :: (PosStream s, Monad m) => (Span (Pos s) -> a -> b) -> ParserT l s e m a -> ParserT l s e m b
spanParser f p = do
  start <- get
  val <- p
  end <- get
  pure (f (Span (streamViewPos start) (streamViewPos end)) val)

-- | Gets the current stream position
getStreamPos :: (PosStream s, Monad m) => ParserT l s e m (Pos s)
getStreamPos = gets streamViewPos
