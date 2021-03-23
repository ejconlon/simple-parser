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
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Data.Char (digitToInt, isDigit, isSpace)
import Data.List (foldl')
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import SimpleParser.Input (dropTokensWhile, dropTokensWhile1, foldTokensWhile, matchToken, satisfyToken,
                           takeTokensWhile1)
import SimpleParser.Parser (ParserT, defaultParser, greedyStarParser, optionalParser)
import SimpleParser.Stream (Chunked (..), Stream (..))

-- | Yields the maximal list of separated items. May return an empty list.
sepByParser :: Monad m =>
  -- | How to parse item
  ParserT e s m a ->
  -- | How to parse separator
  ParserT e s m () ->
  ParserT e s m [a]
sepByParser thing sep = do
  ma <- optionalParser thing
  case ma of
    Nothing -> pure []
    Just a -> do
      as <- greedyStarParser (sep *> thing)
      pure (a : as)

-- | Parses between start and end markers.
betweenParser :: Monad m =>
  -- | How to parse start
  ParserT e s m () ->
  -- | How to parse end
  ParserT e s m () ->
  -- | How to parse inside
  ParserT e s m a ->
  ParserT e s m a
betweenParser start end thing = do
  start
  a <- thing
  end
  pure a

-- | A wrapper for lexemes (equivalent to Megaparsec's 'lexeme').
lexemeParser :: Monad m =>
  -- | How to consume white space after lexeme
  ParserT e s m () ->
  -- | How to parse actual lexeme
  ParserT e s m a ->
  ParserT e s m a
lexemeParser spc p = p <* spc

-- | Consumes a newline character.
newlineParser :: (Stream s, Token s ~ Char, Monad m) => ParserT e s m ()
newlineParser = void (matchToken '\n')

-- | Consumes 0 or more space characters.
spaceParser :: (Stream s, Token s ~ Char, Monad m) => ParserT e s m ()
spaceParser = void (dropTokensWhile isSpace)

isHSpace :: Char -> Bool
isHSpace c = isSpace c && c /= '\n' && c /= '\r'

-- | Consumes 0 or more non-line-break space characters
hspaceParser :: (Stream s, Token s ~ Char, Monad m) => ParserT e s m ()
hspaceParser = void (dropTokensWhile isHSpace)

-- | Consumes 1 or more space characters.
spaceParser1 :: (Stream s, Token s ~ Char, Monad m) => ParserT e s m ()
spaceParser1 = void (dropTokensWhile1 isSpace)

-- | Consumes 1 or more non-line-break space characters
hspaceParser1 :: (Stream s, Token s ~ Char, Monad m) => ParserT e s m ()
hspaceParser1 = void (dropTokensWhile1 isHSpace)

-- | Parses an integer in decimal representation (equivalent to Megaparsec's 'decimal').
decimalParser :: (Stream s, Token s ~ Char, Monad m, Num a) => ParserT e s m a
decimalParser = fmap mkNum (takeTokensWhile1 isDigit) where
  mkNum = foldl' step 0 . chunkToTokens
  step a c = a * 10 + fromIntegral (digitToInt c)

data SP = SP !Integer !Int

dotDecimalParser :: (Stream s, Token s ~ Char, Monad m) => Integer -> ParserT e s m SP
dotDecimalParser c' = do
  void (matchToken '.')
  let mkNum = foldl' step (SP c' 0) . chunkToTokens
      step (SP a e') c = SP (a * 10 + fromIntegral (digitToInt c)) (e' - 1)
  fmap mkNum (takeTokensWhile1 isDigit)

exponentParser :: (Stream s, Token s ~ Char, Monad m) => Int -> ParserT e s m Int
exponentParser e' = do
  void (satisfyToken (\c -> c == 'e' || c == 'E'))
  fmap (+ e') (signedParser (pure ()) decimalParser)

-- | Parses a floating point value as a 'Scientific' number (equivalent to Megaparsec's 'scientific').
scientificParser :: (Stream s, Token s ~ Char, Monad m) => ParserT e s m Scientific
scientificParser = do
  c' <- decimalParser
  SP c e' <- defaultParser (SP c' 0) (dotDecimalParser c')
  e <- defaultParser e' (exponentParser e')
  pure (Sci.scientific c e)

-- | Parses an optional sign character followed by a number and yields a correctly-signed
-- number (equivalend to Megaparsec's 'signed').
signedParser :: (Stream s, Token s ~ Char, Monad m, Num a) =>
  -- | How to consume white space after the sign
  ParserT e s m () ->
  -- | How to parse the number itself
  ParserT e s m a ->
  -- | Parser for signed numbers
  ParserT e s m a
signedParser spc p = defaultParser id (lexemeParser spc sign) <*> p where
  sign = (id <$ matchToken '+') <|> (negate <$ matchToken '-')

data Pair = Pair ![Char] !Bool

-- | Given a quote charcter (like a single or double quote), yields the contents of the
-- string bounded by those quotes. The contents may contain backslash-escaped quotes.
-- Returns nothing if outside quotes are missing or the stream ends before unquote.
escapedStringParser :: (Stream s, Token s ~ Char, Monad m) => Char -> ParserT e s m (Chunk s)
escapedStringParser quoteChar =
  let quoteParser = void (matchToken quoteChar)
      accParser = foldTokensWhile go (Pair [] False)
      innerParser = fmap (\(Pair acc _) -> tokensToChunk (reverse acc)) accParser
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
