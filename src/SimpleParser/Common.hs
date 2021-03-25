-- | Common parsers.
-- See <https://hackage.haskell.org/package/megaparsec-9.0.1/docs/Text-Megaparsec-Char-Lexer.html Text.Megaparsec.Char.Lexer>.
module SimpleParser.Common
  ( sepByParser
  , betweenParser
  , lexemeParser
  , newlineParser
  , spaceParser
  , hspaceParser
  , spaceParser1
  , hspaceParser1
  , decimalParser
  , scientificParser
  , signedParser
  , escapedStringParser
  , spanParser
  , getStreamPos
  ) where

import Control.Monad (void)
import Control.Monad.State (get, gets)
import Data.Char (digitToInt, isDigit, isSpace)
import Data.List (foldl')
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import SimpleParser.Chunked (Chunked (..))
import SimpleParser.Input (dropTokensWhile, dropTokensWhile1, foldTokensWhile, matchToken, takeTokensWhile1)
import SimpleParser.Parser (ParserT, defaultSuccessParser, greedyStarParser, optionalParser, orParser)
import SimpleParser.Stream (Span (..), Stream (..), StreamWithPos (..))

-- | Yields the maximal list of separated items. May return an empty list.
sepByParser :: (Chunked seq elem, Monad m) =>
  -- | How to parse item
  ParserT r s e m elem ->
  -- | How to parse separator
  ParserT r s e m () ->
  ParserT r s e m seq
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
  ParserT r s e m () ->
  -- | How to parse end
  ParserT r s e m () ->
  -- | How to parse inside
  ParserT r s e m a ->
  ParserT r s e m a
betweenParser start end thing = do
  start
  a <- thing
  end
  pure a

-- | A wrapper for lexemes (equivalent to Megaparsec's 'lexeme').
lexemeParser :: Monad m =>
  -- | How to consume white space after lexeme
  ParserT r s e m () ->
  -- | How to parse actual lexeme
  ParserT r s e m a ->
  ParserT r s e m a
lexemeParser spc p = p <* spc

-- | Consumes a newline character.
newlineParser :: (Stream s, Token s ~ Char, Monad m) => ParserT r s e m ()
newlineParser = void (matchToken '\n')

-- | Consumes 0 or more space characters.
spaceParser :: (Stream s, Token s ~ Char, Monad m) => ParserT r s e m ()
spaceParser = void (dropTokensWhile isSpace)

isHSpace :: Char -> Bool
isHSpace c = isSpace c && c /= '\n' && c /= '\r'

-- | Consumes 0 or more non-line-break space characters
hspaceParser :: (Stream s, Token s ~ Char, Monad m) => ParserT r s e m ()
hspaceParser = void (dropTokensWhile isHSpace)

-- | Consumes 1 or more space characters.
spaceParser1 :: (Stream s, Token s ~ Char, Monad m) => ParserT r s e m ()
spaceParser1 = void (dropTokensWhile1 isSpace)

-- | Consumes 1 or more non-line-break space characters
hspaceParser1 :: (Stream s, Token s ~ Char, Monad m) => ParserT r s e m ()
hspaceParser1 = void (dropTokensWhile1 isHSpace)

-- | Parses an integer in decimal representation (equivalent to Megaparsec's 'decimal').
decimalParser :: (Stream s, Token s ~ Char, Monad m, Num a) => ParserT r s e m a
decimalParser = fmap mkNum (takeTokensWhile1 isDigit) where
  mkNum = foldl' step 0 . chunkToTokens
  step a c = a * 10 + fromIntegral (digitToInt c)

data SP = SP !Integer !Int

dotDecimalParser :: (Stream s, Token s ~ Char, Monad m) => Integer -> ParserT r s e m SP
dotDecimalParser c' = do
  void (matchToken '.')
  let mkNum = foldl' step (SP c' 0) . chunkToTokens
      step (SP a e') c = SP (a * 10 + fromIntegral (digitToInt c)) (e' - 1)
  fmap mkNum (takeTokensWhile1 isDigit)

exponentParser :: (Stream s, Token s ~ Char, Monad m) => Int -> ParserT r s e m Int
exponentParser e' = do
  void (orParser (matchToken 'e') (matchToken 'E'))
  fmap (+ e') (signedParser (pure ()) decimalParser)

-- | Parses a floating point value as a 'Scientific' number (equivalent to Megaparsec's 'scientific').
scientificParser :: (Stream s, Token s ~ Char, Monad m) => ParserT r s e m Scientific
scientificParser = do
  c' <- decimalParser
  SP c e' <- defaultSuccessParser (SP c' 0) (dotDecimalParser c')
  e <- defaultSuccessParser e' (exponentParser e')
  pure (Sci.scientific c e)

-- | Parses an optional sign character followed by a number and yields a correctly-signed
-- number (equivalend to Megaparsec's 'signed').
signedParser :: (Stream s, Token s ~ Char, Monad m, Num a) =>
  -- | How to consume white space after the sign
  ParserT r s e m () ->
  -- | How to parse the number itself
  ParserT r s e m a ->
  -- | Parser for signed numbers
  ParserT r s e m a
signedParser spc p = defaultSuccessParser id (lexemeParser spc sign) <*> p where
  sign = orParser (id <$ matchToken '+') (negate <$ matchToken '-')

data Pair = Pair ![Char] !Bool

-- | Given a quote charcter (like a single or double quote), yields the contents of the
-- string bounded by those quotes. The contents may contain backslash-escaped quotes.
-- Returns nothing if outside quotes are missing or the stream ends before unquote.
escapedStringParser :: (Stream s, Token s ~ Char, Monad m) => Char -> ParserT r s e m (Chunk s)
escapedStringParser quoteChar =
  let quoteParser = void (matchToken quoteChar)
      accParser = foldTokensWhile go (Pair [] False)
      innerParser = fmap (\(Pair acc _) -> revTokensToChunk acc) accParser
      escChar = '\\'
      go c (Pair acc esc)
        | c == escChar =
          if esc
            then (True, Pair (escChar:acc) True) -- Append last esc
            else (True, Pair acc True) -- Skip appending this esc
        | c == quoteChar =
          if esc
            then (True, Pair (c:acc) False) -- Escaped quote
            else (False, Pair acc False) -- End of quote
        | otherwise =
          if esc
            then (True, Pair (c:escChar:acc) False) -- Was a non-quote esc, add back
            else (True, Pair (c:acc) False) -- Just consume char
  in betweenParser quoteParser quoteParser innerParser

-- | Adds span information to parsed values.
spanParser :: (StreamWithPos p s, Monad m) => (Span p -> a -> b) -> ParserT r s e m a -> ParserT r s e m b
spanParser f p = do
  start <- get
  val <- p
  end <- get
  pure (f (Span (viewStreamPos start) (viewStreamPos end)) val)

-- | Gets the current stream position
getStreamPos :: (StreamWithPos p s, Monad m) => ParserT r s e m p
getStreamPos = gets viewStreamPos
