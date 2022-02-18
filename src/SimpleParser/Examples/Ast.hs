{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parses SExp-formatted ASTs
module SimpleParser.Examples.Ast where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import Data.Text (Text)
import SimpleParser (Chunk, EmbedTextLabel (..), ExplainLabel (..), MatchBlock (..), MatchCase (MatchCase), Parser,
                     TextLabel, TextualStream, anyToken, anyWord, consumeMatch, greedyStarParser, lexemeParser,
                     lookAheadMatch, spaceParser, throwParser)
import qualified Text.Builder as TB

data AstLabel =
    AstLabelEmbedText !TextLabel
  | AstLabelCtorList
  | AstLabelCtorHead
  | AstLabelCtorBody !Text
  deriving (Eq, Show)

instance ExplainLabel AstLabel where
  explainLabel sl =
    case sl of
      AstLabelEmbedText tl -> explainLabel tl
      AstLabelCtorList -> "constructor list"
      AstLabelCtorHead -> "constructor head"
      AstLabelCtorBody t -> "constructor body (" <> TB.text t <> ")"

instance EmbedTextLabel AstLabel where
  embedTextLabel = AstLabelEmbedText

type AstParserC s = (TextualStream s, Chunk s ~ Text)
type AstParserM s e a = Parser AstLabel s e a

data CtorRes e a =
    CtorFail !String
  | CtorErr !e
  | CtorVal !a
  deriving stock (Eq, Ord, Show)

embedCtorRes :: CtorRes e a -> AstParserM s e a
embedCtorRes = \case
  CtorFail msg -> fail msg
  CtorErr err -> throwParser err
  CtorVal val -> pure val

data Ctor s e t where
  Ctor1 :: (a -> CtorRes e t) -> AstParserM s e a -> Ctor s e t
  Ctor2 :: (a -> b -> CtorRes e t) -> AstParserM s e a -> AstParserM s e b -> Ctor s e t
  Ctor3 :: (a -> b -> c -> CtorRes e t) -> AstParserM s e a -> AstParserM s e b -> AstParserM s e c -> Ctor s e t
  Ctor4 :: (a -> b -> c -> d -> CtorRes e t) -> AstParserM s e a -> AstParserM s e b -> AstParserM s e c -> AstParserM s e d -> Ctor s e t
  Ctor5 :: (a -> b -> c -> d -> x -> CtorRes e t) -> AstParserM s e a -> AstParserM s e b -> AstParserM s e c -> AstParserM s e d -> AstParserM s e x -> Ctor s e t
  CtorN :: (Seq a -> CtorRes e t) -> AstParserM s e a -> Ctor s e t

type CtorDefns s e t = Map Text (Ctor s e t)

data Defns s e t = Defns
  { defAtoms :: AstParserM s e t
  , defCtors :: CtorDefns s e t
  }

spaceP :: AstParserC s => AstParserM s e ()
spaceP = spaceParser

lexP :: AstParserC s => AstParserM s e a -> AstParserM s e a
lexP = lexemeParser spaceP

wordP :: AstParserC s => Maybe AstLabel -> AstParserM s e Text
wordP = lexP . anyWord

astParser :: AstParserC s => AstParserM s e t -> (AstParserM s e t -> CtorDefns s e t) -> AstParserM s e t
astParser mkAtom mkCtors = let p = recAstParser (Defns mkAtom (mkCtors p)) in p

recAstParser :: AstParserC s => Defns s e t -> AstParserM s e t
recAstParser defns = lookAheadMatch block where
  block = MatchBlock anyToken (lexP (defAtoms defns))
    [ MatchCase (Just AstLabelCtorList) (== '(') (ctorDefnsAstParser (defCtors defns))
    ]

ctorDefnsAstParser :: AstParserC s => CtorDefns s e t -> AstParserM s e t
ctorDefnsAstParser ctors = consumeMatch block where
  block = MatchBlock (wordP (Just AstLabelCtorHead)) (fail "Could not match contstructor") cases
  cases = flip fmap (Map.toList ctors) $ \(t, c) ->
    MatchCase (Just (AstLabelCtorBody t)) (== t) (ctorAstParser c)

ctorAstParser :: AstParserC s => Ctor s e t -> AstParserM s e t
ctorAstParser = \case
  Ctor1 f pa -> do
    a <- lexP pa
    embedCtorRes (f a)
  Ctor2 f pa pb -> do
    a <- lexP pa
    b <- lexP pb
    embedCtorRes (f a b)
  Ctor3 f pa pb pc -> do
    a <- lexP pa
    b <- lexP pb
    c <- lexP pc
    embedCtorRes (f a b c)
  Ctor4 f pa pb pc pd -> do
    a <- lexP pa
    b <- lexP pb
    c <- lexP pc
    d <- lexP pd
    embedCtorRes (f a b c d)
  Ctor5 f pa pb pc pd px -> do
    a <- lexP pa
    b <- lexP pb
    c <- lexP pc
    d <- lexP pd
    x <- lexP px
    embedCtorRes (f a b c d x)
  CtorN f px -> do
    xs <- greedyStarParser (lexP px)
    embedCtorRes (f xs)
