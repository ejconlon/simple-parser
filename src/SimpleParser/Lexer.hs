-- | One approach to lexing - map state directly
module SimpleParser.Lexer
  ( Lexer (..)
  , SimpleLexer
  , simpleLexer
  , applyLexer
  , applySimpleLexer
  ) where

import Data.Kind (Type)
import SimpleParser.Parser (ParserT (..))
import SimpleParser.Result (CompoundError (..), Mark (..), MarkStack, ParseError (..), ParseErrorBundle (..),
                            ParseResult (..), ParseSuccess (..), RawError (..), StreamError (..))
import SimpleParser.Stream (Chunk, Token)

data Lexer l1 l2 s1 s2 e1 e2 (m :: Type -> Type) = Lexer
  { lexerParser :: ParserT l1 s1 e1 m s2
  , lexerMapStateFwd :: s1 -> s2
  , lexerMapStateBwd :: s2 -> s1
  , lexerEmbedToken :: Token s2 -> Token s1
  , lexerEmbedChunk :: Chunk s2 -> Chunk s1
  , lexerEmbedErrBwd :: e2 -> e1
  , lexerEmbedLocBwd :: l2 -> l1
  }

type SimpleLexer l s t e m = Lexer l l s t e e m

simpleLexer :: ParserT l s e m t -> (s -> t) -> (t -> s) -> (Token t -> Token s) -> (Chunk t -> Chunk s) -> SimpleLexer l s t e m
simpleLexer p a b c d = Lexer p a b c d id id

convMarkStack :: Lexer l1 l2 s1 s2 e1 e2 m -> MarkStack l2 s2 -> MarkStack l1 s1
convMarkStack = fmap . convMark

convMark :: Lexer l1 l2 s1 s2 e1 e2 m -> Mark l2 s2 -> Mark l1 s1
convMark lx (Mark m_l2 s2) = Mark (fmap (lexerEmbedLocBwd lx) m_l2) (lexerMapStateBwd lx s2)

convErr :: Lexer l1 l2 s1 s2 e1 e2 m -> ParseError l2 s2 e2 -> ParseError l1 s1 e1
convErr lx (ParseError ms s2 ce) = ParseError (convMarkStack lx ms) (lexerMapStateBwd lx s2) (convComErr lx ce)

convComErr :: Lexer l1 l2 s1 s2 e1 e2 m -> CompoundError s2 e2 -> CompoundError s1 e1
convComErr lx = \case
  CompoundErrorStream se -> CompoundErrorStream (convStreamErr lx se)
  CompoundErrorFail txt -> CompoundErrorFail txt
  CompoundErrorCustom e2 -> CompoundErrorCustom (lexerEmbedErrBwd lx e2)

convStreamErr :: Lexer l1 l2 s1 s2 e1 e2 m -> StreamError s2 -> StreamError s1
convStreamErr lx (StreamError rse) = StreamError $ case rse of
  RawErrorMatchEnd to -> RawErrorMatchEnd (lexerEmbedToken lx to)
  RawErrorAnyToken -> RawErrorAnyToken
  RawErrorAnyChunk -> RawErrorAnyChunk
  RawErrorSatisfyToken mto -> RawErrorSatisfyToken (fmap (lexerEmbedToken lx) mto)
  RawErrorMatchToken to mto -> RawErrorMatchToken (lexerEmbedToken lx to) (fmap (lexerEmbedToken lx) mto)
  RawErrorMatchChunk ch mch -> RawErrorMatchChunk (lexerEmbedChunk lx ch) (fmap (lexerEmbedChunk lx) mch)
  RawErrorTakeTokensWhile1 mto -> RawErrorTakeTokensWhile1 (fmap (lexerEmbedToken lx) mto)
  RawErrorDropTokensWhile1 mto -> RawErrorDropTokensWhile1 (fmap (lexerEmbedToken lx) mto)

convRes :: Lexer l1 l2 s1 s2 e1 e2 m -> ParseResult l2 s2 e2 a -> ParseResult l1 s1 e1 a
convRes lx = \case
  ParseResultError (ParseErrorBundle es) -> ParseResultError (ParseErrorBundle (fmap (convErr lx) es))
  ParseResultSuccess (ParseSuccess x y) -> ParseResultSuccess (ParseSuccess (lexerMapStateBwd lx x) y)

applyLexer :: Functor m => Lexer l1 l2 s1 s2 e1 e2 m -> (ParserT l2 s2 e2 m a -> ParserT l1 s1 e1 m a)
applyLexer lx p = ParserT $ \s1 -> fmap (fmap (convRes lx)) (runParserT p (lexerMapStateFwd lx s1))

applySimpleLexer :: Functor m => SimpleLexer l s t e m -> (ParserT l t e m a -> ParserT l s e m a)
applySimpleLexer = applyLexer
