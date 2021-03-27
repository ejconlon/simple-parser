{-# LANGUAGE OverloadedStrings #-}

module SimpleParser.Examples.Json
  ( Json (..)
  , JsonF (..)
  , JsonLabel (..)
  , JsonParser
  , jsonParser
  , recJsonParser
  ) where

import Control.Monad (void)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import SimpleParser.Chunked (TextualChunked (..))
import SimpleParser.Common (betweenParser, escapedStringParser, exclusiveParser, lexemeParser, scientificParser,
                            sepByParser, spaceParser)
import SimpleParser.Input (matchChunk, matchToken)
import SimpleParser.Parser (ParserT)
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

newtype JsonLabel =
    JsonLabelBranch Text
  deriving (Eq, Show)

type JsonParser l s e m = (l ~ JsonLabel, TextualStream s, Eq (Chunk s), Monad m)

jsonParser :: JsonParser l s e m => ParserT l s e m Json
jsonParser = let p = fmap Json (recJsonParser p) in p

recJsonParser :: JsonParser l s e m => ParserT l s e m a -> ParserT l s e m (JsonF a)
recJsonParser root = exclusiveParser opts where
  pairP = objectPairP root
  opts =
    [ (JsonLabelBranch "object", objectP pairP)
    , (JsonLabelBranch "array" , arrayP root)
    , (JsonLabelBranch "string", stringP)
    , (JsonLabelBranch "num", numP)
    , (JsonLabelBranch "bool", boolP)
    , (JsonLabelBranch "null", nullP)
    ]

spaceP :: JsonParser l s e m => ParserT l s e m ()
spaceP = spaceParser

tokL :: JsonParser l s e m => Char -> ParserT l s e m ()
tokL c = lexemeParser spaceP (void (matchToken c))

chunkL :: JsonParser l s e m => Text -> ParserT l s e m ()
chunkL cs = lexemeParser spaceP (void (matchChunk (unpackChunk cs)))

openBraceP, closeBraceP, commaP, colonP, openBracketP, closeBracketP, closeQuoteP :: JsonParser l s e m => ParserT l s e m ()
openBraceP = tokL '{'
closeBraceP = tokL '}'
commaP = tokL ','
colonP = tokL ':'
openBracketP = tokL '['
closeBracketP = tokL ']'
closeQuoteP = tokL '"'

openQuoteP :: JsonParser l s e m => ParserT l s e m ()
openQuoteP = void (matchToken '"')

nullTokP, trueTokP, falseTokP :: JsonParser l s e m => ParserT l s e m ()
nullTokP = chunkL "null"
trueTokP = chunkL "true"
falseTokP = chunkL "false"

rawStringP :: JsonParser l s e m => ParserT l s e m Text
rawStringP = fmap packChunk (escapedStringParser '"')

stringP :: JsonParser l s e m => ParserT l s e m (JsonF a)
stringP = fmap JsonString rawStringP

nullP :: JsonParser l s e m => ParserT l s e m (JsonF a)
nullP = JsonNull <$ nullTokP

boolP :: JsonParser l s e m => ParserT l s e m (JsonF a)
boolP = exclusiveParser
  [ (JsonLabelBranch "true", JsonBool True <$ trueTokP)
  , (JsonLabelBranch "false", JsonBool False <$ falseTokP)
  ]

numP :: JsonParser l s e m => ParserT l s e m (JsonF a)
numP = fmap JsonNum scientificParser

objectPairP :: JsonParser l s e m => ParserT l s e m a -> ParserT l s e m (Text, a)
objectPairP root = do
  name <- rawStringP
  colonP
  value <- root
  pure (name, value)

objectP :: JsonParser l s e m => ParserT l s e m (Text, a) -> ParserT l s e m (JsonF a)
objectP pairP = betweenParser openBraceP closeBraceP (fmap JsonObject (sepByParser pairP commaP))

arrayP :: JsonParser l s e m => ParserT l s e m a -> ParserT l s e m (JsonF a)
arrayP root = betweenParser openBracketP closeBracketP (fmap JsonArray (sepByParser root commaP))
