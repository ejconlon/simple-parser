module SimpleParser.Chunked
  ( Chunked (..)
  , TextualChunked (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.List (uncons)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Word (Word8)
import Text.Builder (Builder)
import qualified Text.Builder as TB

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

  -- | Some datatypes (like 'Seq') may admit a "better" implementation
  -- for building a chunk in reverse.
  revTokensToChunk :: [token] -> chunk
  revTokensToChunk = tokensToChunk . reverse

-- | Captures textual streams.
class Chunked chunk Char => TextualChunked chunk where
  buildChunk :: chunk -> Builder
  buildChunk = TB.text . packChunk
  buildChunkLazy :: chunk -> TLB.Builder
  buildChunkLazy = TLB.fromLazyText . packChunkLazy
  packChunk :: chunk -> Text
  packChunk = TB.run . buildChunk
  packChunkLazy :: chunk -> TL.Text
  packChunkLazy = TLB.toLazyText . buildChunkLazy
  unpackChunk :: Text -> chunk
  unpackChunkLazy :: TL.Text -> chunk
  {-# MINIMAL (buildChunk | packChunk), (buildChunkLazy | packChunkLazy), unpackChunk, unpackChunkLazy #-}

instance Chunked [a] a where
  consChunk = (:)
  unconsChunk = uncons
  tokenToChunk a = [a]
  tokensToChunk = id
  chunkToTokens = id
  chunkLength = length
  chunkEmpty = null

instance (a ~ Char) => TextualChunked [a] where
  buildChunk = TB.string
  buildChunkLazy = TLB.fromString
  packChunk = T.pack
  packChunkLazy = TL.pack
  unpackChunk = T.unpack
  unpackChunkLazy = TL.unpack

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
  revTokensToChunk = foldr (flip (:|>)) Empty

instance (a ~ Char) => TextualChunked (Seq a) where
  buildChunk = TB.string . toList
  buildChunkLazy = TLB.fromString . toList
  packChunk = T.pack . toList
  packChunkLazy = TL.pack . toList
  unpackChunk = Seq.fromList . T.unpack
  unpackChunkLazy = Seq.fromList . TL.unpack

instance Chunked Text Char where
  consChunk = T.cons
  unconsChunk = T.uncons
  tokenToChunk = T.singleton
  tokensToChunk = T.pack
  chunkToTokens = T.unpack
  chunkLength = T.length
  chunkEmpty = T.null

instance TextualChunked Text where
  buildChunk = TB.text
  buildChunkLazy = TLB.fromText
  packChunk = id
  packChunkLazy = TL.fromStrict
  unpackChunk = id
  unpackChunkLazy = TL.toStrict

instance Chunked TL.Text Char where
  consChunk = TL.cons
  unconsChunk = TL.uncons
  tokenToChunk = TL.singleton
  tokensToChunk = TL.pack
  chunkToTokens = TL.unpack
  chunkLength = fromIntegral . TL.length
  chunkEmpty = TL.null

instance TextualChunked TL.Text where
  buildChunk = TB.text . TL.toStrict
  buildChunkLazy = TLB.fromLazyText
  packChunk = TL.toStrict
  packChunkLazy = id
  unpackChunk = TL.fromStrict
  unpackChunkLazy = id

instance Chunked ByteString Word8 where
  consChunk = BS.cons
  unconsChunk = BS.uncons
  tokenToChunk = BS.singleton
  tokensToChunk = BS.pack
  chunkToTokens = BS.unpack
  chunkLength = BS.length
  chunkEmpty = BS.null

instance Chunked BSL.ByteString Word8 where
  consChunk = BSL.cons
  unconsChunk = BSL.uncons
  tokenToChunk = BSL.singleton
  tokensToChunk = BSL.pack
  chunkToTokens = BSL.unpack
  chunkLength = fromIntegral . BSL.length
  chunkEmpty = BSL.null
