-- | This reworks 'Text.Megaparsec.Stream' to split interfaces.
-- See <https://hackage.haskell.org/package/megaparsec-9.0.1/docs/Text-Megaparsec-Stream.html Text.Megaparsec.Stream>.
module SimpleParser.Stream
  ( Stream (..)
  , TextualStream
  , defaultStreamDropN
  , defaultStreamDropWhile
  , Offset (..)
  , OffsetStream (..)
  , newOffsetStream
  , Line (..)
  , Col (..)
  , LinePos (..)
  , LinePosStream (..)
  , newLinePosStream
  , Span (..)
  ) where

import Data.Bifunctor (first, second)
import Data.Kind (Type)
import Data.List (foldl')
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import SimpleParser.Chunked (Chunked (..), TextualChunked (..))

-- | 'Stream' lets us peel off tokens and chunks for parsing
-- with explicit state passing.
class Chunked (Chunk s) (Token s) => Stream s where
  type family Chunk s :: Type
  type family Token s :: Type
  type family Pos s :: Type

  streamViewPos :: s -> Pos s

  streamTake1 :: s -> Maybe (Token s, s)
  streamTakeN :: Int -> s -> Maybe (Chunk s, s)
  streamTakeWhile :: (Token s -> Bool) -> s -> (Chunk s, s)

  streamDropN :: Int -> s -> Maybe (Int, s)
  streamDropN = defaultStreamDropN

  streamDropWhile :: (Token s -> Bool) -> s -> (Int, s)
  streamDropWhile = defaultStreamDropWhile

defaultStreamDropN :: Stream s => Int -> s -> Maybe (Int, s)
defaultStreamDropN n = fmap (first chunkLength) . streamTakeN n

defaultStreamDropWhile :: Stream s => (Token s -> Bool) -> s -> (Int, s)
defaultStreamDropWhile pcate = first chunkLength . streamTakeWhile pcate

type TextualStream s = (Stream s, Token s ~ Char, TextualChunked (Chunk s))

instance Stream [a] where
  type instance Chunk [a] = [a]
  type instance Token [a] = a
  type instance Pos [a] = ()

  streamViewPos = const ()
  streamTake1 = unconsChunk
  streamTakeN n s
    | n <= 0 = Just ([], s)
    | null s = Nothing
    | otherwise = Just (splitAt n s)
  streamTakeWhile = span

instance Stream (Seq a) where
  type instance Chunk (Seq a) = Seq a
  type instance Token (Seq a) = a
  type instance Pos (Seq a) = ()

  streamViewPos = const ()
  streamTake1 = unconsChunk
  streamTakeN n s
    | n <= 0 = Just (Seq.empty, s)
    | Seq.null s = Nothing
    | otherwise = Just (Seq.splitAt n s)
  streamTakeWhile = Seq.spanl

  -- TODO(ejconlon) Specialize drops

instance Stream Text where
  type instance Chunk Text = Text
  type instance Token Text = Char
  type instance Pos Text = ()

  streamViewPos = const ()
  streamTake1 = T.uncons
  streamTakeN n s
    | n <= 0 = Just (T.empty, s)
    | T.null s = Nothing
    | otherwise = Just (T.splitAt n s)
  streamTakeWhile = T.span

newtype Offset = Offset { unOffset :: Int }
  deriving newtype (Eq, Show, Ord, Enum, Num, Real, Integral)

-- | Stream wrapper that maintains an offset position.
data OffsetStream s = OffsetStream
  { osOffset :: !Offset
  , osState :: !s
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Stream s => Stream (OffsetStream s) where
  type instance Chunk (OffsetStream s) = Chunk s
  type instance Token (OffsetStream s) = Token s
  type instance Pos (OffsetStream s) = Offset

  streamViewPos (OffsetStream o _) = o
  streamTake1 (OffsetStream o s) = fmap (second (OffsetStream (succ o))) (streamTake1 s)
  streamTakeN n (OffsetStream (Offset x) s) = fmap go (streamTakeN n s) where
    go (a, b) = (a, OffsetStream (Offset (x + chunkLength a)) b)
  streamTakeWhile pcate (OffsetStream (Offset x) s) =
    let (a, b) = streamTakeWhile pcate s
    in (a, OffsetStream (Offset (x + chunkLength a)) b)
  streamDropN n (OffsetStream (Offset x) s) = fmap go (streamDropN n s) where
    go (m, b) = (m, OffsetStream (Offset (x + m)) b)
  streamDropWhile pcate (OffsetStream (Offset x) s) =
    let (m, b) = streamDropWhile pcate s
    in (m, OffsetStream (Offset (x + m)) b)

newOffsetStream :: s -> OffsetStream s
newOffsetStream = OffsetStream 0

newtype Line = Line { unLine :: Int }
  deriving newtype (Eq, Show, Ord, Enum, Num, Real, Integral)

newtype Col = Col { unCol :: Int }
  deriving newtype (Eq, Show, Ord, Enum, Num, Real, Integral)

-- | A 0-based line/col position in a character-based stream.
data LinePos = LinePos
  { lpOffset :: !Offset
  , lpLine :: !Line
  , lpCol :: !Col
  } deriving (Eq, Show, Ord)

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
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Stream s, Token s ~ Char) => Stream (LinePosStream s) where
  type instance Chunk (LinePosStream s) = Chunk s
  type instance Token (LinePosStream s) = Token s
  type instance Pos (LinePosStream s) = LinePos

  streamViewPos (LinePosStream p _) = p
  streamTake1 (LinePosStream p s) = fmap (\(a, b) -> (a, LinePosStream (incrLinePosToken p a) b)) (streamTake1 s)
  streamTakeN n (LinePosStream p s) = fmap go (streamTakeN n s) where
    go (a, b) = (a, LinePosStream (incrLinePosChunk p (chunkToTokens a)) b)
  streamTakeWhile pcate (LinePosStream p s) =
    let (a, b) = streamTakeWhile pcate s
    in (a, LinePosStream (incrLinePosChunk p (chunkToTokens a)) b)

  -- Drops can't be specialized because we need to examine each character for newlines.

newLinePosStream :: s -> LinePosStream s
newLinePosStream = LinePosStream initLinePos

-- | A range between two positions.
data Span p = Span
  { spanStart :: !p
  , spanEnd :: !p
  } deriving (Eq, Show, Ord)
