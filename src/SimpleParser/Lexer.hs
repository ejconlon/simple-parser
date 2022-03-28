-- | Utilities for handling lexing/tokenization as a separate parsing pass
module SimpleParser.Lexer
  ( Spanned (..)
  , LexedStream (..)
  , LexedSpan (..)
  , spannedParser
  , lexedParser
  , runParserLexed
  , lexedParseInteractive
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State.Strict (gets)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import SimpleParser.Explain (ExplainError, ExplainLabel, TextBuildable)
import SimpleParser.Input (matchEnd)
import SimpleParser.Interactive (ErrorStyle (..), renderInteractive)
import SimpleParser.Parser (Parser, ParserT, greedyStarParser, runParser)
import SimpleParser.Result (ParseResult (..), ParseSuccess (..))
import SimpleParser.Stream (HasLinePos (..), LinePosStream, PosStream (..), Span (..), Stream (..), newLinePosStream)
import SimpleParser.Throw (runParserEnd)

-- | A value annotated with a 'Span'
data Spanned p a = Spanned
  { spannedSpan :: !(Span p)
  , spannedValue :: !a
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- | A materialized sequence of 'Spanned' values
data LexedStream p a = LexedStream
  { lsTokens :: !(Seq (Spanned p a))
  , lsEndPos :: !p
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

newtype LexedChunk a = LexedChunk { unLexedChunk :: Seq a }
  deriving stock (Show)
  deriving newtype (Eq)

instance Stream (LexedStream p a) where
  type Token (LexedStream p a) = a
  type Chunk (LexedStream p a) = Seq a

  streamTake1 (LexedStream ss ep) =
    case ss of
      Empty -> Nothing
      Spanned _ a :<| tl -> Just (a, LexedStream tl ep)

  streamTakeN n s@(LexedStream ss ep)
    | n <= 0 = Just (Seq.empty, s)
    | Seq.null ss = Nothing
    | otherwise =
        let (out, rest) = Seq.splitAt n ss
        in Just (fmap spannedValue out, LexedStream rest ep)

  streamTakeWhile f (LexedStream ss ep) =
    let (out, rest) = Seq.spanl (f . spannedValue) ss
    in (fmap spannedValue out, LexedStream rest ep)

  -- TODO(ejconlon) Specialize drops

-- | Position in a 'LexedStream'
data LexedSpan p =
    LexedSpanElem !(Span p)
  | LexedSpanEnd !p
  deriving stock (Eq, Show)

instance HasLinePos p => HasLinePos (LexedSpan p) where
  viewLine = \case
    LexedSpanElem sp -> viewLine (spanStart sp)
    LexedSpanEnd p -> viewLine p
  viewCol = \case
    LexedSpanElem sp -> viewCol (spanStart sp)
    LexedSpanEnd p -> viewCol p

instance PosStream (LexedStream p a) where
  type Pos (LexedStream p a) = LexedSpan p

  streamViewPos (LexedStream ss ep) =
    case ss of
      Empty -> LexedSpanEnd ep
      Spanned sp _ :<| _ -> LexedSpanElem sp

-- | Annotates parse result with a span
spannedParser :: (PosStream s, Monad m) => ParserT l s e m a -> ParserT l s e m (Spanned (Pos s) a)
spannedParser p = do
  p1 <- gets streamViewPos
  a <- p
  p2 <- gets streamViewPos
  pure (Spanned (Span p1 p2) a)

-- | Given a parser for a single token, repeatedly apply it and annotate results with spans
lexedParser :: (PosStream s, Monad m) => ParserT l s e m a -> ParserT l s e m (LexedStream (Pos s) a)
lexedParser p = LexedStream <$> greedyStarParser (spannedParser p) <*> gets streamViewPos

-- | Similar to 'runParserEnd' - first lexes the entire stream, applies the given cleanup function,
-- then runs the second parser over the results
runParserLexed :: (
  Typeable l1, Typeable e1, Typeable s, Typeable (Token s), Typeable (Chunk s),
  Show l1, Show e1, Show s, Show (Token s), Show (Chunk s),
  Typeable l2, Typeable e2, Typeable (Pos s), Typeable a,
  Show l2, Show e2, Show (Pos s), Show a,
  PosStream s, MonadThrow m) => Parser l1 s e1 a -> (LexedStream (Pos s) a -> LexedStream (Pos s) a) -> Parser l2 (LexedStream (Pos s) a) e2 b -> s -> m b
runParserLexed lp f p s = do
  ls <- runParserEnd (lexedParser lp) s
  runParserEnd p (f ls)

-- | Similar to 'parseInteractive'
lexedParseInteractive :: (
  s ~ LinePosStream Text, TextBuildable a,
  ExplainLabel l1, ExplainError e1, ExplainLabel l2, ExplainError e2) =>
  Parser l1 s e1 a -> (LexedStream (Pos s) a -> LexedStream (Pos s) a) -> Parser l2 (LexedStream (Pos s) a) e2 b -> String -> IO (Maybe b)
lexedParseInteractive lp f p input = do
  -- TODO renderInteractive TWICE
  let lexRes = runParser (lexedParser lp <* matchEnd) (newLinePosStream (T.pack input))
  putStrLn "Lex result:"
  renderInteractive ErrorStyleErrata input lexRes
  case lexRes of
    Just (ParseResultSuccess (ParseSuccess _ ls)) -> do
      let parseRes = runParser (p <* matchEnd) (f ls)
      putStrLn "Parse result:"
      renderInteractive ErrorStyleErrata input parseRes
      let res = case parseRes of { Just (ParseResultSuccess (ParseSuccess _ b)) -> Just b; _ -> Nothing }
      pure res
    _ -> pure Nothing
