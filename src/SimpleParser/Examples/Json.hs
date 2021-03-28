{-# LANGUAGE OverloadedStrings #-}

module SimpleParser.Examples.Json
  ( Json (..)
  , JsonF (..)
  , JsonLabel (..)
  , JsonParserC
  , JsonParserM
  , jsonParser
  , recJsonParser
  ) where

import Control.Monad (void)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Void (Void)
import SimpleParser.Chunked (TextualChunked (..))
import SimpleParser.Common (EmbedTextLabel (..), TextLabel, betweenParser, escapedStringParser, exclusiveParser,
                            lexemeParser, scientificParser, sepByParser, spaceParser)
import SimpleParser.Explain (ExplainLabel (..))
import SimpleParser.Input (matchChunk, matchToken)
import SimpleParser.Parser (Parser)
import SimpleParser.Stream (Stream (..), TextualStream)
import qualified Text.Builder as TB

data JsonF a =
    JsonObject !(Seq (Text, a))
  | JsonArray !(Seq a)
  | JsonString !Text
  | JsonBool !Bool
  | JsonNum !Scientific
  | JsonNull
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Json = Json { unJson :: JsonF Json } deriving (Eq, Show)

data JsonLabel =
    JsonLabelBranch !Text
  | JsonLabelEmbedText !TextLabel
  deriving (Eq, Show)

instance ExplainLabel JsonLabel where
  explainLabel jl =
    case jl of
      JsonLabelBranch b -> "branch " <> TB.text b
      JsonLabelEmbedText tl -> explainLabel tl

instance EmbedTextLabel JsonLabel where
  embedTextLabel = JsonLabelEmbedText

type JsonParserC s = (TextualStream s, Eq (Chunk s))

type JsonParserM s a = Parser JsonLabel s Void a

jsonParser :: JsonParserC s => JsonParserM s Json
jsonParser = let p = fmap Json (recJsonParser p) in p

recJsonParser :: JsonParserC s => JsonParserM s a -> JsonParserM s (JsonF a)
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

spaceP :: JsonParserC s => JsonParserM s ()
spaceP = spaceParser

tokL :: JsonParserC s => Char -> JsonParserM s ()
tokL c = lexemeParser spaceP (void (matchToken c))

chunkL :: JsonParserC s => Text -> JsonParserM s ()
chunkL cs = lexemeParser spaceP (void (matchChunk (unpackChunk cs)))

openBraceP, closeBraceP, commaP, colonP, openBracketP, closeBracketP, closeQuoteP :: JsonParserC s => JsonParserM s ()
openBraceP = tokL '{'
closeBraceP = tokL '}'
commaP = tokL ','
colonP = tokL ':'
openBracketP = tokL '['
closeBracketP = tokL ']'
closeQuoteP = tokL '"'

openQuoteP :: JsonParserC s => JsonParserM s ()
openQuoteP = void (matchToken '"')

nullTokP, trueTokP, falseTokP :: JsonParserC s => JsonParserM s ()
nullTokP = chunkL "null"
trueTokP = chunkL "true"
falseTokP = chunkL "false"

rawStringP :: JsonParserC s => JsonParserM s Text
rawStringP = fmap packChunk (escapedStringParser '"')

stringP :: JsonParserC s => JsonParserM s (JsonF a)
stringP = fmap JsonString rawStringP

nullP :: JsonParserC s => JsonParserM s (JsonF a)
nullP = JsonNull <$ nullTokP

boolP :: JsonParserC s => JsonParserM s (JsonF a)
boolP = exclusiveParser
  [ (JsonLabelBranch "true", JsonBool True <$ trueTokP)
  , (JsonLabelBranch "false", JsonBool False <$ falseTokP)
  ]

numP :: JsonParserC s => JsonParserM s (JsonF a)
numP = fmap JsonNum scientificParser

objectPairP :: JsonParserC s => JsonParserM s a -> JsonParserM s (Text, a)
objectPairP root = do
  name <- rawStringP
  colonP
  value <- root
  pure (name, value)

objectP :: JsonParserC s => JsonParserM s (Text, a) -> JsonParserM s (JsonF a)
objectP pairP = betweenParser openBraceP closeBraceP (fmap JsonObject (sepByParser pairP commaP))

arrayP :: JsonParserC s => JsonParserM s a -> JsonParserM s (JsonF a)
arrayP root = betweenParser openBracketP closeBracketP (fmap JsonArray (sepByParser root commaP))
