module SimpleParser.Examples.Json
  ( Json (..)
  , JsonF (..)
  , parseJson
  ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Foldable (asum)
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

type JsonInput a = Input Char Void StringStreamState a

parseJson :: String -> [Json]
parseJson str = do
  ParseResult v _ <- runInput jsonInput stringStream (newStringStreamState str)
  case v of
    ParseSuccess a -> pure a

jsonSpace :: JsonInput ()
jsonSpace = greedyStarInput_ (void (satisfyInput isSpace))

jsonLexeme :: JsonInput () -> JsonInput a -> JsonInput a
jsonLexeme spaceAfter thing = do
  a <- thing
  spaceAfter
  pure a

jsonBetween :: JsonInput () -> JsonInput () -> JsonInput a -> JsonInput a
jsonBetween start end thing = do
  start
  a <- thing
  end
  pure a

jsonSepBy :: JsonInput a -> JsonInput () -> JsonInput [a]
jsonSepBy thing sep = go [] where
  optThing = optionalInput thing
  optSep = optionalInput sep
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

jsonCharLexeme :: Char -> JsonInput ()
jsonCharLexeme c = void (jsonLexeme jsonSpace (charInput c))

jsonWordLexeme :: String -> JsonInput ()
jsonWordLexeme cs = void (jsonLexeme jsonSpace (wordInput_ cs))

openBrace, closeBrace, comma, colon, openBracket, closeBracket, closeQuote :: JsonInput ()
openBrace = jsonCharLexeme '{'
closeBrace = jsonCharLexeme '}'
comma = jsonCharLexeme ','
colon = jsonCharLexeme ':'
openBracket = jsonCharLexeme '['
closeBracket = jsonCharLexeme ']'
closeQuote = jsonCharLexeme '"'

openQuote :: JsonInput ()
openQuote = void (charInput '"')

nullTok, trueTok, falseTok :: JsonInput ()
nullTok = jsonWordLexeme "null"
trueTok = jsonWordLexeme "true"
falseTok = jsonWordLexeme "false"

nonQuoteChar :: JsonInput Char
nonQuoteChar = satisfyInput (/= '"')

nonQuoteString :: JsonInput String
nonQuoteString = greedyStarInput nonQuoteChar

rawStringInput :: JsonInput String
rawStringInput = jsonBetween openQuote closeQuote nonQuoteString

-- NOTE: Does not handle escape codes. Use `foldWhileInput` for that...
stringInput :: JsonInput (JsonF a)
stringInput = fmap JsonString rawStringInput

nullInput :: JsonInput (JsonF a)
nullInput = JsonNull <$ nullTok

boolInput :: JsonInput (JsonF a)
boolInput = branchInput [JsonBool True <$ trueTok, JsonBool False <$ falseTok]

objectPairInput :: JsonInput a -> JsonInput (String, a)
objectPairInput root = do
  name <- rawStringInput
  colon
  value <- root
  pure (name, value)

objectInput :: JsonInput (String, a) -> JsonInput (JsonF a)
objectInput pairInput = jsonBetween openBrace closeBrace (fmap JsonObject (jsonSepBy pairInput comma))

arrayInput :: JsonInput a -> JsonInput (JsonF a)
arrayInput root = jsonBetween openBracket closeBracket (fmap JsonArray (jsonSepBy root comma))

rootInput :: JsonInput a -> JsonInput (JsonF a)
rootInput root = asum opts where
  pairInput = objectPairInput root
  opts =
    [ objectInput pairInput
    , arrayInput root
    , stringInput
    , boolInput
    , nullInput
    ]

jsonInput :: JsonInput Json
jsonInput = let p = fmap Json (rootInput p) in p
