{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parses Sexp-formatted ASTs
module SimpleParser.Examples.Direct.Ast
  ( AstLabel (..)
  , AstParserC
  , AstParserM
  , CtorRes (..)
  , Ctor (..)
  , CtorDefns
  , astParser
  , lexAstParser
  , identAstParser
  ) where

import Control.Monad (ap, void)
import Control.Monad.Except (MonadError (..))
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import Data.Text (Text)
import SimpleParser (Chunk, EmbedTextLabel (..), ExplainLabel (..), MatchBlock (..), MatchCase (MatchCase), Parser,
                     TextLabel, TextualStream, anyToken, betweenParser, consumeMatch, greedyStarParser, lexemeParser,
                     lookAheadMatch, matchToken, spaceParser, takeTokensWhile1, throwParser)
import qualified Text.Builder as TB

data AstLabel =
    AstLabelEmbedText !TextLabel
  | AstLabelCtorList
  | AstLabelCtorHead
  | AstLabelCtorBody !Text
  | AstLabelCustom !Text
  deriving (Eq, Show)

instance ExplainLabel AstLabel where
  explainLabel sl =
    case sl of
      AstLabelEmbedText tl -> explainLabel tl
      AstLabelCtorList -> "constructor list"
      AstLabelCtorHead -> "constructor head"
      AstLabelCtorBody t -> "constructor body (" <> TB.text t <> ")"
      AstLabelCustom t -> "custom: " <> TB.text t

instance EmbedTextLabel AstLabel where
  embedTextLabel = AstLabelEmbedText

type AstParserC s = (TextualStream s, Chunk s ~ Text)
type AstParserM s e a = Parser AstLabel s e a

data CtorRes e a =
    CtorResFail !String
  | CtorResErr !e
  | CtorResVal !a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative (CtorRes e) where
  pure = CtorResVal
  (<*>) = ap

instance Monad (CtorRes e) where
  return = pure
  r >>= f =
    case r of
      CtorResFail msg -> CtorResFail msg
      CtorResErr err -> CtorResErr err
      CtorResVal val -> f val

instance MonadFail (CtorRes e) where
  fail = CtorResFail

instance MonadError e (CtorRes e) where
  throwError = CtorResErr
  catchError r h =
    case r of
      CtorResFail msg -> CtorResFail msg
      CtorResErr err -> h err
      CtorResVal val -> CtorResVal val

embedCtorRes :: CtorRes e a -> AstParserM s e a
embedCtorRes = \case
  CtorResFail msg -> fail msg
  CtorResErr err -> throwParser err
  CtorResVal val -> pure val

data Ctor s e t where
  Ctor0 :: CtorRes e t -> Ctor s e t
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

lexAstParser :: AstParserC s => AstParserM s e a -> AstParserM s e a
lexAstParser = lexemeParser spaceP

openParenP :: AstParserC s => AstParserM s e ()
openParenP = lexAstParser (void (matchToken '('))

closeParenP :: AstParserC s => AstParserM s e ()
closeParenP = lexAstParser (void (matchToken ')'))

nonDelimPred :: Char -> Bool
nonDelimPred c = c /= '(' && c /= ')' && not (isSpace c)

identAstParser :: AstParserC s => Maybe AstLabel -> AstParserM s e Text
identAstParser = lexAstParser . flip takeTokensWhile1 nonDelimPred

astParser :: AstParserC s => AstParserM s e t -> (AstParserM s e t -> CtorDefns s e t) -> AstParserM s e t
astParser mkAtom mkCtors = let p = recAstParser (Defns mkAtom (mkCtors p)) in p

recAstParser :: AstParserC s => Defns s e t -> AstParserM s e t
recAstParser defns = lookAheadMatch block where
  block = MatchBlock anyToken (lexAstParser (defAtoms defns))
    [ MatchCase (Just AstLabelCtorList) (== '(') (ctorDefnsAstParser (defCtors defns))
    ]

ctorDefnsAstParser :: AstParserC s => CtorDefns s e t -> AstParserM s e t
ctorDefnsAstParser ctors = betweenParser openParenP closeParenP (consumeMatch block) where
  block = MatchBlock (identAstParser (Just AstLabelCtorHead)) (fail "Could not match constructor") cases
  cases = flip fmap (Map.toList ctors) $ \(t, c) ->
    MatchCase (Just (AstLabelCtorBody t)) (== t) (ctorAstParser c)

ctorAstParser :: AstParserC s => Ctor s e t -> AstParserM s e t
ctorAstParser = \case
  Ctor0 r -> embedCtorRes r
  Ctor1 f pa -> do
    a <- lexAstParser pa
    embedCtorRes (f a)
  Ctor2 f pa pb -> do
    a <- lexAstParser pa
    b <- lexAstParser pb
    embedCtorRes (f a b)
  Ctor3 f pa pb pc -> do
    a <- lexAstParser pa
    b <- lexAstParser pb
    c <- lexAstParser pc
    embedCtorRes (f a b c)
  Ctor4 f pa pb pc pd -> do
    a <- lexAstParser pa
    b <- lexAstParser pb
    c <- lexAstParser pc
    d <- lexAstParser pd
    embedCtorRes (f a b c d)
  Ctor5 f pa pb pc pd px -> do
    a <- lexAstParser pa
    b <- lexAstParser pb
    c <- lexAstParser pc
    d <- lexAstParser pd
    x <- lexAstParser px
    embedCtorRes (f a b c d x)
  CtorN f px -> do
    xs <- greedyStarParser (lexAstParser px)
    embedCtorRes (f xs)
