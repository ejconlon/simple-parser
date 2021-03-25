{-# LANGUAGE OverloadedStrings #-}

module SimpleParser.Examples.Json
  ( Json (..)
  , JsonF (..)
  , JsonParser
  , jsonParser
  , recJsonParser
  ) where

import Control.Monad (void)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import SimpleParser.Chunked (TextualChunked (..))
import SimpleParser.Common (betweenParser, escapedStringParser, lexemeParser, scientificParser, sepByParser,
                            spaceParser)
import SimpleParser.Input (matchChunk, matchToken)
import SimpleParser.Parser (ParserT, andAllParser, isolateParser)
import SimpleParser.Stream (Stream (..), TextualStream)

data JsonF a =
    JsonObject !(Seq (Text, a))
  | JsonArray !(Seq a)
  | JsonString !Text
  | JsonBool !Bool
  | JsonNum !Scientific
  | JsonNull
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Json = Json { unJson :: JsonF Json } deriving (Eq, Show)

type JsonParser s m = (TextualStream s, Eq (Chunk s), Monad m)

jsonParser :: JsonParser s m => ParserT r s e m Json
jsonParser = let p = fmap Json (recJsonParser p) in p

recJsonParser :: JsonParser s m => ParserT r s e m a -> ParserT r s e m (JsonF a)
recJsonParser root = isolateParser (andAllParser opts) where
  pairP = objectPairP root
  opts =
    [ objectP pairP
    , arrayP root
    , stringP
    , numP
    , boolP
    , nullP
    ]

spaceP :: JsonParser s m => ParserT r s e m ()
spaceP = spaceParser

tokL :: JsonParser s m => Char -> ParserT r s e m ()
tokL c = lexemeParser spaceP (void (matchToken c))

chunkL :: JsonParser s m => Text -> ParserT r s e m ()
chunkL cs = lexemeParser spaceP (void (matchChunk (unpackChunk cs)))

openBraceP, closeBraceP, commaP, colonP, openBracketP, closeBracketP, closeQuoteP :: JsonParser s m => ParserT r s e m ()
openBraceP = tokL '{'
closeBraceP = tokL '}'
commaP = tokL ','
colonP = tokL ':'
openBracketP = tokL '['
closeBracketP = tokL ']'
closeQuoteP = tokL '"'

openQuoteP :: JsonParser s m => ParserT r s e m ()
openQuoteP = void (matchToken '"')

nullTokP, trueTokP, falseTokP :: JsonParser s m => ParserT r s e m ()
nullTokP = chunkL "null"
trueTokP = chunkL "true"
falseTokP = chunkL "false"

rawStringP :: JsonParser s m => ParserT r s e m Text
rawStringP = fmap packChunk (escapedStringParser '"')

stringP :: JsonParser s m => ParserT r s e m (JsonF a)
stringP = fmap JsonString rawStringP

nullP :: JsonParser s m => ParserT r s e m (JsonF a)
nullP = JsonNull <$ nullTokP

boolP :: JsonParser s m => ParserT r s e m (JsonF a)
boolP = isolateParser (andAllParser [JsonBool True <$ trueTokP, JsonBool False <$ falseTokP])

numP :: JsonParser s m => ParserT r s e m (JsonF a)
numP = fmap JsonNum scientificParser

objectPairP :: JsonParser s m => ParserT r s e m a -> ParserT r s e m (Text, a)
objectPairP root = do
  name <- rawStringP
  colonP
  value <- root
  pure (name, value)

objectP :: JsonParser s m => ParserT r s e m (Text, a) -> ParserT r s e m (JsonF a)
objectP pairP = betweenParser openBraceP closeBraceP (fmap JsonObject (sepByParser pairP commaP))

arrayP :: JsonParser s m => ParserT r s e m a -> ParserT r s e m (JsonF a)
arrayP root = betweenParser openBracketP closeBracketP (fmap JsonArray (sepByParser root commaP))
