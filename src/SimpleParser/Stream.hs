-- | This reworks 'Text.Megaparsec.Stream' to split interfaces.
-- See <https://hackage.haskell.org/package/megaparsec-9.0.1/docs/Text-Megaparsec-Stream.html Text.Megaparsec.Stream>.
module SimpleParser.Stream
  ( Chunked (..)
  , Stream (..)
  , defaultStreamDropN
  , defaultStreamDropWhile
  , OffsetStream (..)
  , newOffsetStream
  ) where

import Data.Bifunctor (first, second)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List (uncons)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

-- | 'Chunked' captures the basic relationship between tokens and chunks of them.
-- Basically, these things behave like lists, sequences, text, etc.
class Monoid chunk => Chunked chunk token | chunk -> token where
  consChunk :: token -> chunk -> chunk
  unconsChunk :: chunk -> Maybe (token, chunk)
  tokenToChunk :: token -> chunk
  tokensToChunk :: [token] -> chunk
  chunkToTokens :: chunk -> [token]
  chunkLength :: chunk -> Int
  chunkEmpty :: chunk -> Bool

-- TODO(ejconlon) Add instances for Strict BS, Lazy BS, and Lazy Text

instance Chunked [a] a where
  consChunk = (:)
  unconsChunk = uncons
  tokenToChunk a = [a]
  tokensToChunk = id
  chunkToTokens = id
  chunkLength = length
  chunkEmpty = null

instance Chunked (Seq a) a where
  consChunk = (:<|)
  unconsChunk s =
    case s of
      Empty -> Nothing
      a :<| b -> Just (a, b)
  tokenToChunk = Seq.singleton
  tokensToChunk = Seq.fromList
  chunkToTokens = toList
  chunkLength = Seq.length
  chunkEmpty = Seq.null

instance Chunked Text Char where
  consChunk = T.cons
  unconsChunk = T.uncons
  tokenToChunk = T.singleton
  tokensToChunk = T.pack
  chunkToTokens = T.unpack
  chunkLength = T.length
  chunkEmpty = T.null

-- | 'Stream' lets us peel off tokens and chunks for parsing
-- with explicit state passing.
class Chunked (Chunk s) (Token s) => Stream s where
  type family Chunk s :: Type
  type family Token s :: Type

  streamTake1 :: s -> Maybe (Token s, s)
  streamTakeN :: Int -> s -> Maybe (Chunk s, s)
  streamTakeWhile :: (Token s -> Bool) -> s -> (Chunk s, s)

  streamDropN :: Int -> s -> Maybe (Int, s)
  streamDropN = defaultStreamDropN

  streamDropWhile :: (Token s -> Bool) -> s -> (Int, s)
  streamDropWhile = defaultStreamDropWhile

-- TODO(ejconlon) Specialize drops

defaultStreamDropN :: Stream s => Int -> s -> Maybe (Int, s)
defaultStreamDropN n = fmap (first chunkLength) . streamTakeN n

defaultStreamDropWhile :: Stream s => (Token s -> Bool) -> s -> (Int, s)
defaultStreamDropWhile pcate = first chunkLength . streamTakeWhile pcate

instance Stream [a] where
  type instance Chunk [a] = [a]
  type instance Token [a] = a

  streamTake1 l =
    case l of
      [] -> Nothing
      t:ts -> Just (t, ts)
  streamTakeN n s
    | n <= 0 = Just ([], s)
    | null s = Nothing
    | otherwise = Just (splitAt n s)
  streamTakeWhile = span

instance Stream (Seq a) where
  type instance Chunk (Seq a) = Seq a
  type instance Token (Seq a) = a

  streamTake1 s =
    case s of
      Empty -> Nothing
      t :<| ts -> Just (t, ts)
  streamTakeN n s
    | n <= 0 = Just (Seq.empty, s)
    | Seq.null s = Nothing
    | otherwise = Just (Seq.splitAt n s)
  streamTakeWhile = Seq.spanl

instance Stream Text where
  type instance Chunk Text = Text
  type instance Token Text = Char

  streamTake1 = T.uncons
  streamTakeN n s
    | n <= 0 = Just (T.empty, s)
    | T.null s = Nothing
    | otherwise = Just (T.splitAt n s)
  streamTakeWhile = T.span

data OffsetStream s = OffsetStream
  { osOffset :: !Int
  , osState :: !s
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Stream s => Stream (OffsetStream s) where
  type instance Chunk (OffsetStream s) = Chunk s
  type instance Token (OffsetStream s) = Token s

  streamTake1 (OffsetStream o s) = fmap (second (OffsetStream (succ o))) (streamTake1 s)
  streamTakeN n (OffsetStream o s) = fmap go (streamTakeN n s) where
    go (a, b) = (a, OffsetStream (o + chunkLength a) b)
  streamTakeWhile pcate (OffsetStream o s) =
    let (a, b) = streamTakeWhile pcate s
    in (a, OffsetStream (o + chunkLength a) b)
  streamDropN n (OffsetStream o s) = fmap go (streamDropN n s) where
    go (m, b) = (m, OffsetStream (o + m) b)
  streamDropWhile pcate (OffsetStream o s) =
    let (m, b) = streamDropWhile pcate s
    in (m, OffsetStream (o + m) b)

newOffsetStream :: s -> OffsetStream s
newOffsetStream = OffsetStream 0
