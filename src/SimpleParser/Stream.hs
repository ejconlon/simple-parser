-- | This reworks 'Text.Megaparsec.Stream' to split interfaces.
-- See <https://hackage.haskell.org/package/megaparsec-9.0.1/docs/Text-Megaparsec-Stream.html Text.Megaparsec.Stream>.
module SimpleParser.Stream
  ( Stream (..)
  , TextualStream
  , PosStream (..)
  , Offset (..)
  , OffsetStream (..)
  , newOffsetStream
  , Line (..)
  , Col (..)
  , LinePos (..)
  , LinePosStream (..)
  , newLinePosStream
  , Span (..)
  , HasLinePos (..)
  )
where

import Data.Bifunctor (first, second)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Kind (Type)
import Data.List (foldl')
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word (Word8)
import SimpleParser.Chunked (Chunked (..), TextualChunked (..))

-- | 'Stream' lets us peel off tokens and chunks for parsing with explicit state passing.
class Chunked (Chunk s) (Token s) => Stream s where
  type Chunk s :: Type
  type Token s :: Type

  streamTake1 :: s -> Maybe (Token s, s)

  streamTakeN :: Int -> s -> Maybe (Chunk s, s)
  streamTakeN = go mempty
   where
    ret acc s = Just (revTokensToChunk acc, s)
    go !acc !n !s
      | n <= 0 = ret acc s
      | otherwise =
          case streamTake1 s of
            Nothing -> if null acc then Nothing else ret acc s
            Just (t, s') -> go (t : acc) (n - 1) s'

  streamTakeWhile :: (Token s -> Bool) -> s -> (Chunk s, s)
  streamTakeWhile p = go mempty
   where
    go !acc !s =
      case streamTake1 s of
        Just (t, s') | p t -> go (t : acc) s'
        _ -> (revTokensToChunk acc, s)

  streamDropN :: Int -> s -> Maybe (Int, s)
  streamDropN n = fmap (first chunkLength) . streamTakeN n

  streamDropWhile :: (Token s -> Bool) -> s -> (Int, s)
  streamDropWhile pcate = first chunkLength . streamTakeWhile pcate

type TextualStream s = (Stream s, Token s ~ Char, TextualChunked (Chunk s))

instance Stream [a] where
  type Chunk [a] = [a]
  type Token [a] = a

  streamTake1 = unconsChunk
  streamTakeN n s
    | n <= 0 = Just ([], s)
    | null s = Nothing
    | otherwise = Just (splitAt n s)
  streamTakeWhile = span

instance Stream (Seq a) where
  type Chunk (Seq a) = Seq a
  type Token (Seq a) = a

  streamTake1 = unconsChunk
  streamTakeN n s
    | n <= 0 = Just (Seq.empty, s)
    | Seq.null s = Nothing
    | otherwise = Just (Seq.splitAt n s)
  streamTakeWhile = Seq.spanl

-- TODO(ejconlon) Specialize drops

instance Stream Text where
  type Chunk Text = Text
  type Token Text = Char

  streamTake1 = T.uncons
  streamTakeN n s
    | n <= 0 = Just (T.empty, s)
    | T.null s = Nothing
    | otherwise = Just (T.splitAt n s)
  streamTakeWhile = T.span

-- TODO(ejconlon) Specialize drops

instance Stream TL.Text where
  type Chunk TL.Text = TL.Text
  type Token TL.Text = Char

  streamTake1 = TL.uncons
  streamTakeN n s
    | n <= 0 = Just (TL.empty, s)
    | TL.null s = Nothing
    | otherwise = Just (TL.splitAt (fromIntegral n) s)
  streamTakeWhile = TL.span

-- TODO(ejconlon) Specialize drops

instance Stream ByteString where
  type Chunk ByteString = ByteString
  type Token ByteString = Word8

  streamTake1 = BS.uncons
  streamTakeN n s
    | n <= 0 = Just (BS.empty, s)
    | BS.null s = Nothing
    | otherwise = Just (BS.splitAt n s)
  streamTakeWhile = BS.span

-- TODO(ejconlon) Specialize drops

instance Stream BSL.ByteString where
  type Chunk BSL.ByteString = BSL.ByteString
  type Token BSL.ByteString = Word8

  streamTake1 = BSL.uncons
  streamTakeN n s
    | n <= 0 = Just (BSL.empty, s)
    | BSL.null s = Nothing
    | otherwise = Just (BSL.splitAt (fromIntegral n) s)
  streamTakeWhile = BSL.span

-- TODO(ejconlon) Specialize drops

-- | 'PosStream' adds position tracking to a 'Stream'.
class Stream s => PosStream s where
  type Pos s :: Type

  streamViewPos :: s -> Pos s

newtype Offset = Offset {unOffset :: Int}
  deriving newtype (Eq, Show, Ord, Enum, Num, Real, Integral)

-- | Stream wrapper that maintains an offset position.
data OffsetStream s = OffsetStream
  { osOffset :: !Offset
  , osState :: !s
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Stream s => Stream (OffsetStream s) where
  type Chunk (OffsetStream s) = Chunk s
  type Token (OffsetStream s) = Token s

  streamTake1 (OffsetStream o s) = fmap (second (OffsetStream (succ o))) (streamTake1 s)
  streamTakeN n (OffsetStream (Offset x) s) = fmap go (streamTakeN n s)
   where
    go (a, b) = (a, OffsetStream (Offset (x + chunkLength a)) b)
  streamTakeWhile pcate (OffsetStream (Offset x) s) =
    let (a, b) = streamTakeWhile pcate s
    in  (a, OffsetStream (Offset (x + chunkLength a)) b)
  streamDropN n (OffsetStream (Offset x) s) = fmap go (streamDropN n s)
   where
    go (m, b) = (m, OffsetStream (Offset (x + m)) b)
  streamDropWhile pcate (OffsetStream (Offset x) s) =
    let (m, b) = streamDropWhile pcate s
    in  (m, OffsetStream (Offset (x + m)) b)

instance Stream s => PosStream (OffsetStream s) where
  type Pos (OffsetStream s) = Offset

  streamViewPos (OffsetStream o _) = o

newOffsetStream :: s -> OffsetStream s
newOffsetStream = OffsetStream 0

newtype Line = Line {unLine :: Int}
  deriving newtype (Eq, Show, Ord, Enum, Num, Real, Integral)

newtype Col = Col {unCol :: Int}
  deriving newtype (Eq, Show, Ord, Enum, Num, Real, Integral)

-- | A 0-based line/col position in a character-based stream.
data LinePos = LinePos
  { lpOffset :: !Offset
  , lpLine :: !Line
  , lpCol :: !Col
  }
  deriving (Eq, Show, Ord)

-- | The canonical initial position.
initLinePos :: LinePos
initLinePos = LinePos 0 0 0

incrLinePosToken :: LinePos -> Char -> LinePos
incrLinePosToken (LinePos o l c) z
  | z == '\n' = LinePos (succ o) (succ l) 0
  | otherwise = LinePos (succ o) l (succ c)

incrLinePosChunk :: LinePos -> [Char] -> LinePos
incrLinePosChunk = foldl' incrLinePosToken

-- | Stream wrapper that maintains a line/col position.
data LinePosStream s = LinePosStream
  { lpsLinePos :: !LinePos
  , lpsState :: !s
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Stream s, Token s ~ Char) => Stream (LinePosStream s) where
  type Chunk (LinePosStream s) = Chunk s
  type Token (LinePosStream s) = Token s

  streamTake1 (LinePosStream p s) = fmap (\(a, b) -> (a, LinePosStream (incrLinePosToken p a) b)) (streamTake1 s)
  streamTakeN n (LinePosStream p s) = fmap go (streamTakeN n s)
   where
    go (a, b) = (a, LinePosStream (incrLinePosChunk p (chunkToTokens a)) b)
  streamTakeWhile pcate (LinePosStream p s) =
    let (a, b) = streamTakeWhile pcate s
    in  (a, LinePosStream (incrLinePosChunk p (chunkToTokens a)) b)

-- Drops can't be specialized because we need to examine each character for newlines.

instance (Stream s, Token s ~ Char) => PosStream (LinePosStream s) where
  type Pos (LinePosStream s) = LinePos

  streamViewPos (LinePosStream p _) = p

newLinePosStream :: s -> LinePosStream s
newLinePosStream = LinePosStream initLinePos

-- | A range between two positions.
data Span p = Span
  { spanStart :: !p
  , spanEnd :: !p
  }
  deriving (Eq, Show, Ord)

-- | Allows projections into (Line, Col) for more exotic stream positions.
class HasLinePos p where
  viewLine :: p -> Line
  viewCol :: p -> Col

instance HasLinePos LinePos where
  viewLine = lpLine
  viewCol = lpCol
