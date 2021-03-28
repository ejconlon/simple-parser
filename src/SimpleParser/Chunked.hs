module SimpleParser.Chunked
  ( Chunked (..)
  , TextualChunked (..)
  ) where

import Data.Foldable (toList)
import Data.List (uncons)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
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
  packChunk :: chunk -> Text
  packChunk = TB.run . buildChunk
  unpackChunk :: Text -> chunk

-- TODO(ejconlon) Add instances for Strict BS, Lazy BS, and Lazy Text

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
  packChunk = T.pack
  unpackChunk = T.unpack

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
  packChunk = T.pack . toList
  unpackChunk = Seq.fromList . T.unpack

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
  packChunk = id
  unpackChunk = id
