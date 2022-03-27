-- | Utilities for handling lexing/tokenization as a separate parsing pass
module SimpleParser.Lexer
  ( Spanned (..)
  , LexedStream (..)
  , LexedSpan (..)
  , spannedParser
  , lexedParser
  , runParserLexed
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State.Strict (gets)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Typeable (Typeable)
import SimpleParser.Parser (Parser, ParserT, greedyStarParser)
import SimpleParser.Stream (PosStream (..), Span (Span), Stream (..))
import SimpleParser.Throw (runParserEnd)

-- | A value annotated with a 'Span'
data Spanned p a = Spanned
  { spannedSpan :: !(Span p)
  , spannedValue :: !a
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- | A materialized sequence of 'Spanned' values
newtype LexedStream p a = LexedStream { unLexedStream :: Seq (Spanned p a) }
  deriving stock (Show, Functor, Foldable, Traversable)
  deriving newtype (Eq)

instance Stream (LexedStream p a) where
  type Token (LexedStream p a) = a
  type Chunk (LexedStream p a) = Seq a

  streamTake1 (LexedStream ss) =
    case ss of
      Empty -> Nothing
      Spanned _ a :<| tl -> Just (a, LexedStream tl)

  streamTakeN n s@(LexedStream ss)
    | n <= 0 = Just (Seq.empty, s)
    | Seq.null ss = Nothing
    | otherwise =
        let (out, rest) = Seq.splitAt n ss
        in Just (fmap spannedValue out, LexedStream rest)

  streamTakeWhile f (LexedStream ss) =
    let (out, rest) = Seq.spanl (f . spannedValue) ss
    in (fmap spannedValue out, LexedStream rest)

  -- TODO(ejconlon) Specialize drops

-- | Position in a 'LexedStream'
data LexedSpan p =
    LexedSpanNext !(Span p)
  | LexedSpanEnd
  deriving stock (Eq, Show)

instance PosStream (LexedStream p a) where
  type Pos (LexedStream p a) = LexedSpan p

  streamViewPos (LexedStream ss) =
    case ss of
      Empty -> LexedSpanEnd
      Spanned sp _ :<| _ -> LexedSpanNext sp

-- | Annotates parse result with a span
spannedParser :: (PosStream s, Monad m) => ParserT l s e m a -> ParserT l s e m (Spanned (Pos s) a)
spannedParser p = do
  p1 <- gets streamViewPos
  a <- p
  p2 <- gets streamViewPos
  pure (Spanned (Span p1 p2) a)

-- | Given a parser for a single token, repeatedly apply it and annotate results with spans
lexedParser :: (PosStream s, Monad m) => ParserT l s e m a -> ParserT l s e m (LexedStream (Pos s) a)
lexedParser p = fmap LexedStream (greedyStarParser (spannedParser p))

-- | Similar to 'runParserEnd' - first lexes the entire stream then runs the second parser over the results
runParserLexed :: (
  Typeable l1, Typeable e1, Typeable s, Typeable (Token s), Typeable (Chunk s),
  Show l1, Show e1, Show s, Show (Token s), Show (Chunk s),
  Typeable l2, Typeable e2, Typeable (Pos s), Typeable a,
  Show l2, Show e2, Show (Pos s), Show a,
  PosStream s, MonadThrow m) => Parser l1 s e1 a -> Parser l2 (LexedStream (Pos s) a) e2 b -> s -> m b
runParserLexed lp p s = do
  ls <- runParserEnd (lexedParser lp) s
  runParserEnd p ls
