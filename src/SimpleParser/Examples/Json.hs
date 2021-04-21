{-# LANGUAGE OverloadedStrings #-}

module SimpleParser.Examples.Json
  ( Json (..)
  , JsonF (..)
  , JsonParserC
  , JsonParserM
  , jsonParser
  , recJsonParser
  ) where

import Control.Monad (void)
import Data.Foldable (asum)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Void (Void)
import SimpleParser (Parser, Stream (..), TextLabel, TextualChunked (..), TextualStream, betweenParser, commitParser,
                     escapedStringParser, lexemeParser, matchChunk, matchToken, onEmptyParser, orParser, satisfyToken,
                     scientificParser, sepByParser, signedNumStartPred, spaceParser)

data JsonF a =
    JsonObject !(Seq (Text, a))
  | JsonArray !(Seq a)
  | JsonString !Text
  | JsonBool !Bool
  | JsonNum !Scientific
  | JsonNull
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Json = Json { unJson :: JsonF Json } deriving (Eq, Show)

type JsonParserC s = (TextualStream s, Eq (Chunk s))

type JsonParserM s a = Parser TextLabel s Void a

jsonParser :: JsonParserC s => JsonParserM s Json
jsonParser = let p = fmap Json (recJsonParser p) in p

isBoolStartPred :: Char -> Bool
isBoolStartPred c = c == 't' || c == 'f'

recJsonParser :: JsonParserC s => JsonParserM s a -> JsonParserM s (JsonF a)
recJsonParser root = onEmptyParser (asum opts) (fail "failed to parse json document") where
  pairP = objectPairP root
  opts =
    [ commitParser openBraceP (objectP pairP)
    , commitParser openBracketP (arrayP root)
    , commitParser openQuoteP stringP
    , commitParser (void (satisfyToken Nothing signedNumStartPred)) numP
    , commitParser (void (satisfyToken Nothing isBoolStartPred)) boolP
    , commitParser (void (matchToken 'n')) nullP
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
boolP = orParser (JsonBool True <$ trueTokP) (JsonBool False <$ falseTokP)

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
