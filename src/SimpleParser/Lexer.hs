module SimpleParser.Lexer where

import Control.Monad.State.Strict (gets)
import Data.Sequence (Seq (..))
import SimpleParser.Parser (ParserT, greedyStarParser)
import SimpleParser.Stream (PosStream (..), Span (Span), Stream (..))
import qualified Data.Sequence as Seq

data Spanned p a = Spanned
  { spannedSpan :: !(Span p)
  , spannedValue :: !a
  } deriving stock (Eq, Show)

newtype LexedStream p a = LexedStream { unLexedStream :: Seq (Spanned p a) }

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

spannedParser :: (PosStream s, Monad m) => ParserT l s e m a -> ParserT l s e m (Spanned (Pos s) a)
spannedParser p = do
  p1 <- gets streamViewPos
  a <- p
  p2 <- gets streamViewPos
  pure (Spanned (Span p1 p2) a)

lexedParser :: (PosStream s, Monad m) => ParserT l s e m a -> ParserT l s e m (LexedStream (Pos s) a)
lexedParser p = fmap LexedStream (greedyStarParser (spannedParser p))
