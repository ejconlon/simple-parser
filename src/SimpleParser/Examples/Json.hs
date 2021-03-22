{-# LANGUAGE OverloadedStrings #-}

module SimpleParser.Examples.Json
  ( Json (..)
  , JsonF (..)
  , parseJson
  ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Foldable (asum)
import Data.Text (Text)
import Data.Void (Void)
import SimpleParser

-- JSON without numbers...
data JsonF a =
    JsonObject ![(String, a)]
  | JsonArray ![a]
  | JsonString !String
  | JsonBool !Bool
  | JsonNull
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Json = Json { unJson :: JsonF Json } deriving (Eq, Show)

type JsonParser a = Parser Void Text a

parseJson :: Text -> [Json]
parseJson str = do
  ParseResult v _ <- runParser (jsonParser <* matchEnd) str
  case v of
    ParseSuccess a -> pure a

jsonSpace :: JsonParser ()
jsonSpace = greedyStarParser_ (void (satisfyToken isSpace))

jsonLexeme :: JsonParser () -> JsonParser a -> JsonParser a
jsonLexeme spaceAfter thing = do
  a <- thing
  spaceAfter
  pure a

jsonBetween :: JsonParser () -> JsonParser () -> JsonParser a -> JsonParser a
jsonBetween start end thing = do
  start
  a <- thing
  end
  pure a

jsonSepBy :: JsonParser a -> JsonParser () -> JsonParser [a]
jsonSepBy thing sep = go [] where
  optThing = optionalParser thing
  optSep = optionalParser sep
  go !acc = do
    ma <- optThing
    case ma of
      Nothing -> if null acc then pure [] else empty
      Just a -> do
        let newAcc = a:acc
        ms <- optSep
        case ms of
          Nothing -> pure (reverse newAcc)
          Just () -> go newAcc

jsonCharLexeme :: Char -> JsonParser ()
jsonCharLexeme c = void (jsonLexeme jsonSpace (matchToken c))

jsonWordLexeme :: Text -> JsonParser ()
jsonWordLexeme cs = void (jsonLexeme jsonSpace (matchChunk cs))

openBrace, closeBrace, comma, colon, openBracket, closeBracket, closeQuote :: JsonParser ()
openBrace = jsonCharLexeme '{'
closeBrace = jsonCharLexeme '}'
comma = jsonCharLexeme ','
colon = jsonCharLexeme ':'
openBracket = jsonCharLexeme '['
closeBracket = jsonCharLexeme ']'
closeQuote = jsonCharLexeme '"'

openQuote :: JsonParser ()
openQuote = void (matchToken '"')

nullTok, trueTok, falseTok :: JsonParser ()
nullTok = jsonWordLexeme "null"
trueTok = jsonWordLexeme "true"
falseTok = jsonWordLexeme "false"

nonQuoteChar :: JsonParser Char
nonQuoteChar = satisfyToken (/= '"')

nonQuoteString :: JsonParser String
nonQuoteString = greedyStarParser nonQuoteChar

rawStringParser :: JsonParser String
rawStringParser = jsonBetween openQuote closeQuote nonQuoteString

-- TODO(ejconlon) This does not handle escape codes. Use `foldTokensWhile` for that...
stringParser :: JsonParser (JsonF a)
stringParser = fmap JsonString rawStringParser

nullParser :: JsonParser (JsonF a)
nullParser = JsonNull <$ nullTok

boolParser :: JsonParser (JsonF a)
boolParser = branchParser [JsonBool True <$ trueTok, JsonBool False <$ falseTok]

objectPairParser :: JsonParser a -> JsonParser (String, a)
objectPairParser root = do
  name <- rawStringParser
  colon
  value <- root
  pure (name, value)

objectParser :: JsonParser (String, a) -> JsonParser (JsonF a)
objectParser pairParser = jsonBetween openBrace closeBrace (fmap JsonObject (jsonSepBy pairParser comma))

arrayParser :: JsonParser a -> JsonParser (JsonF a)
arrayParser root = jsonBetween openBracket closeBracket (fmap JsonArray (jsonSepBy root comma))

rootParser :: JsonParser a -> JsonParser (JsonF a)
rootParser root = asum opts where
  pairParser = objectPairParser root
  opts =
    [ objectParser pairParser
    , arrayParser root
    , stringParser
    , boolParser
    , nullParser
    ]

jsonParser :: JsonParser Json
jsonParser = let p = fmap Json (rootParser p) in p
