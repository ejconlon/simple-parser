module SimpleParser.CharString
  ( CharString (..)
  , LazyCharString (..)
  , toLazyCharString
  , toStrictCharString
  ) where

import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Coerce (coerce)
import Data.String (IsString)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import SimpleParser.Chunked (Chunked (..), TextualChunked (..))
import SimpleParser.Stream (Stream (..))
import qualified Text.Builder as TB

newtype CharString = CharString
  { unCharString :: ByteString
  } deriving newtype (Eq, Ord, Show, Semigroup, Monoid, IsString)

newtype LazyCharString = LazyCharString
  { unLazyCharString :: BSL.ByteString
  } deriving newtype (Eq, Ord, Show, Semigroup, Monoid, IsString)

toLazyCharString :: CharString -> LazyCharString
toLazyCharString = undefined

toStrictCharString :: LazyCharString -> CharString
toStrictCharString = undefined

instance Chunked CharString Char where
  consChunk a = CharString . BSC.cons a . unCharString
  unconsChunk = fmap (second CharString) . BSC.uncons . unCharString
  tokenToChunk = CharString . BSC.singleton
  tokensToChunk = CharString . BSC.pack
  chunkToTokens = BSC.unpack . unCharString
  chunkLength = BSC.length . unCharString
  chunkEmpty = BSC.null . unCharString

instance TextualChunked CharString where
  buildChunk = TB.asciiByteString . unCharString
  packChunk = TE.decodeLatin1 . unCharString
  unpackChunk = CharString . TE.encodeUtf8

instance Stream CharString where
  type Chunk CharString = CharString
  type Token CharString = Char

  streamTake1 = coerce . BSC.uncons . coerce
  streamTakeN n (CharString s)
    | n <= 0 = Just (coerce (BSC.empty, s))
    | BSC.null s = Nothing
    | otherwise = Just (coerce (BSC.splitAt n s))
  streamTakeWhile = coerce . BSC.span

instance Chunked LazyCharString Char where
  consChunk a = LazyCharString . BSLC.cons a . unLazyCharString
  unconsChunk = fmap (second LazyCharString) . BSLC.uncons . unLazyCharString
  tokenToChunk = LazyCharString . BSLC.singleton
  tokensToChunk = LazyCharString . BSLC.pack
  chunkToTokens = BSLC.unpack . unLazyCharString
  chunkLength = fromIntegral . BSLC.length . unLazyCharString
  chunkEmpty = BSLC.null . unLazyCharString

instance TextualChunked LazyCharString where
  buildChunk = TB.asciiByteString . BSLC.toStrict . unLazyCharString
  packChunk = TL.toStrict . TLE.decodeLatin1 . unLazyCharString
  unpackChunk = LazyCharString . TLE.encodeUtf8 . TL.fromStrict

instance Stream LazyCharString where
  type Chunk LazyCharString = LazyCharString
  type Token LazyCharString = Char

  streamTake1 = coerce . BSLC.uncons . coerce
  streamTakeN n (LazyCharString s)
    | n <= 0 = Just (coerce (BSLC.empty, s))
    | BSLC.null s = Nothing
    | otherwise = Just (coerce (BSLC.splitAt (fromIntegral n) s))
  streamTakeWhile = coerce . BSLC.span
