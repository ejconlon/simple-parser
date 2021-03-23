{-# LANGUAGE OverloadedStrings #-}

module SimpleParser.Examples.Json
  ( Json (..)
  , JsonF (..)
  , parseJson
  , rootJsonParser
  , jsonParser
  ) where

import Control.Monad (void)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Void (Void)
import SimpleParser

-- JSON without numbers...
data JsonF a =
    JsonObject ![(Text, a)]
  | JsonArray ![a]
  | JsonString !Text
  | JsonBool !Bool
  | JsonNum !Scientific
  | JsonNull
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Json = Json { unJson :: JsonF Json } deriving (Eq, Show)

type JsonParser a = Parser Void Text a

parseJson :: Text -> [Json]
parseJson str = do
  ParseResult v _ <- runParser (jsonParser <* matchEnd) str
  case v of
    ParseSuccess a -> pure a

jsonTokenLexeme :: Char -> JsonParser ()
jsonTokenLexeme c = void (lexemeParser spaceParser (matchToken c))

jsonChunkLexeme :: Text -> JsonParser ()
jsonChunkLexeme cs = void (lexemeParser spaceParser (matchChunk cs))

openBrace, closeBrace, comma, colon, openBracket, closeBracket, closeQuote :: JsonParser ()
openBrace = jsonTokenLexeme '{'
closeBrace = jsonTokenLexeme '}'
comma = jsonTokenLexeme ','
colon = jsonTokenLexeme ':'
openBracket = jsonTokenLexeme '['
closeBracket = jsonTokenLexeme ']'
closeQuote = jsonTokenLexeme '"'

openQuote :: JsonParser ()
openQuote = void (matchToken '"')

nullTok, trueTok, falseTok :: JsonParser ()
nullTok = jsonChunkLexeme "null"
trueTok = jsonChunkLexeme "true"
falseTok = jsonChunkLexeme "false"

nonQuoteString :: JsonParser Text
nonQuoteString = takeTokensWhile (/= '"')

rawStringParser :: JsonParser Text
rawStringParser = escapedStringParser '"'

stringParser :: JsonParser (JsonF a)
stringParser = fmap JsonString rawStringParser

nullParser :: JsonParser (JsonF a)
nullParser = JsonNull <$ nullTok

boolParser :: JsonParser (JsonF a)
boolParser = isolateParser (branchParser [JsonBool True <$ trueTok, JsonBool False <$ falseTok])

numParser :: JsonParser (JsonF a)
numParser = fmap JsonNum scientificParser

objectPairParser :: JsonParser a -> JsonParser (Text, a)
objectPairParser root = do
  name <- rawStringParser
  colon
  value <- root
  pure (name, value)

objectParser :: JsonParser (Text, a) -> JsonParser (JsonF a)
objectParser pairParser = betweenParser openBrace closeBrace (fmap JsonObject (sepByParser pairParser comma))

arrayParser :: JsonParser a -> JsonParser (JsonF a)
arrayParser root = betweenParser openBracket closeBracket (fmap JsonArray (sepByParser root comma))

rootJsonParser :: JsonParser a -> JsonParser (JsonF a)
rootJsonParser root = isolateParser (branchParser opts) where
  pairParser = objectPairParser root
  opts =
    [ objectParser pairParser
    , arrayParser root
    , stringParser
    , numParser
    , boolParser
    , nullParser
    ]

jsonParser :: JsonParser Json
jsonParser = let p = fmap Json (rootJsonParser p) in p
