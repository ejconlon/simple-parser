{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module SimpleParser.Explain
  ( TextBuildable (..)
  , ShowTextBuildable (..)
  , ExplainLabel (..)
  , ErrorExplanation (..)
  , ExplainError (..)
  , Explainable
  , ParseErrorExplanation (..)
  , explainParseError
  , buildParseErrorExplanation
  , buildAllParseErrorExplanations
  )
where

import Control.Monad (join)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Void (Void, absurd)
import SimpleParser.Common (CompoundTextLabel (..), TextLabel (..))
import SimpleParser.Result
  ( CompoundError (..)
  , ParseError (..)
  , RawError (..)
  , StreamError (..)
  , parseErrorEnclosingLabels
  , parseErrorNarrowestSpan
  )
import SimpleParser.Stream (HasLinePos (..), PosStream (..), Span (..), Stream (..))
import Text.Builder (Builder)
import qualified Text.Builder as TB

-- | Types that can be rendered into a textual error message
-- (Effectively a fancy Show)
class TextBuildable a where
  buildText :: a -> Builder

instance TextBuildable Char where
  buildText = TB.char

instance TextBuildable String where
  buildText = TB.string

instance TextBuildable Text where
  buildText = TB.text

instance TextBuildable Builder where
  buildText = id

buildTextFromList :: TextBuildable a => [a] -> Builder
buildTextFromList ss = "[" <> TB.intercalate ", " (fmap buildText ss) <> "]"

instance TextBuildable a => TextBuildable [a] where
  buildText = buildTextFromList

instance TextBuildable a => TextBuildable (Seq a) where
  buildText = buildTextFromList . toList

-- | Deriving-Via wrapper for 'TextBuildable' for types with 'Show'
newtype ShowTextBuildable a = ShowTextBuildable {unShowTextBuildable :: a}

instance Show a => TextBuildable (ShowTextBuildable a) where
  buildText = TB.string . show . unShowTextBuildable

class ExplainLabel l where
  explainLabel :: l -> Builder

  explainLabelText :: l -> Text
  explainLabelText = TB.run . explainLabel

instance ExplainLabel Void where
  explainLabel = absurd

instance ExplainLabel TextLabel where
  explainLabel l =
    case l of
      TextLabelSpace -> "space"
      TextLabelHSpace -> "non-line-breaking space"
      TextLabelDigit -> "digit"

instance ExplainLabel l => ExplainLabel (CompoundTextLabel l) where
  explainLabel c =
    case c of
      CompoundTextLabelText l -> explainLabel l
      CompoundTextLabelCustom l -> explainLabel l

data ErrorExplanation = ErrorExplanation
  { eeReason :: !Text
  , eeExpected :: !(Maybe Text)
  , eeActual :: !(Maybe Text)
  }
  deriving (Eq, Show)

class ExplainError e where
  explainError :: e -> ErrorExplanation

instance ExplainError Void where
  explainError = absurd

endMsg :: Text
endMsg = "end of stream"

tokB :: TextBuildable a => a -> Builder
tokB t = "token '" <> buildText t <> "'"

tokT :: TextBuildable a => a -> Text
tokT = TB.run . tokB

mayTokT :: TextBuildable a => Maybe a -> Text
mayTokT = maybe endMsg tokT

chunkB :: TextBuildable a => a -> Builder
chunkB k = "chunk \"" <> buildText k <> "\""

chunkT :: TextBuildable a => a -> Text
chunkT = TB.run . chunkB

mayChunkT :: TextBuildable a => Maybe a -> Text
mayChunkT = maybe endMsg chunkT

instance (TextBuildable (Token s), TextBuildable (Chunk s)) => ExplainError (StreamError s) where
  explainError (StreamError re) =
    case re of
      RawErrorMatchEnd actTok ->
        ErrorExplanation "failed to match end of stream" (Just endMsg) (Just (tokT actTok))
      RawErrorAnyToken ->
        ErrorExplanation "failed to match any token" (Just "any token") (Just endMsg)
      RawErrorAnyChunk ->
        ErrorExplanation "failed to match any chunk" (Just "any chunk") (Just endMsg)
      RawErrorSatisfyToken mayActTok ->
        ErrorExplanation "failed to satisfy token predicate" Nothing (Just (mayTokT mayActTok))
      RawErrorMatchToken expTok mayActTok ->
        ErrorExplanation "failed to match token" (Just (tokT expTok)) (Just (mayTokT mayActTok))
      RawErrorMatchChunk expChunk mayActChunk ->
        ErrorExplanation "failed to match chunk" (Just (chunkT expChunk)) (Just (mayChunkT mayActChunk))
      RawErrorTakeTokensWhile1 mayActTok ->
        ErrorExplanation "failed to take 1 or more tokens" Nothing (Just (mayTokT mayActTok))
      RawErrorDropTokensWhile1 mayActTok ->
        ErrorExplanation "failed to drop 1 or more tokens" Nothing (Just (mayTokT mayActTok))

instance (TextBuildable (Token s), TextBuildable (Chunk s), ExplainError e) => ExplainError (CompoundError s e) where
  explainError ce =
    case ce of
      CompoundErrorStream se -> explainError se
      CompoundErrorFail msg -> ErrorExplanation msg Nothing Nothing
      CompoundErrorCustom e -> explainError e

type Explainable l s e = (PosStream s, ExplainLabel l, ExplainError e)

data ParseErrorExplanation p = ParseErrorExplanation
  { peeSpan :: !(Span p)
  , peeContext :: !(Seq Text)
  , peeDetails :: !(Maybe Text)
  , peeErrExp :: !ErrorExplanation
  }
  deriving (Eq, Show)

explainParseError :: (TextBuildable (Token s), TextBuildable (Chunk s), Explainable l s e) => ParseError l s e -> ParseErrorExplanation (Pos s)
explainParseError pe =
  let (mayLab, sp) = parseErrorNarrowestSpan pe
      context = fmap explainLabelText (parseErrorEnclosingLabels pe)
      mayDetails = fmap explainLabelText mayLab
      errExp = explainError (peError pe)
  in  ParseErrorExplanation sp context mayDetails errExp

buildSpan :: HasLinePos p => Span p -> Builder
buildSpan (Span p1 p2) =
  let l1 = viewLine p1
      c1 = viewCol p1
      l2 = viewLine p2
      c2 = viewCol p2
      r1 = TB.decimal (succ l1) <> ":" <> TB.decimal (succ c1)
      r2 = TB.decimal (succ l2) <> ":" <> TB.decimal (succ c2)
  in  if l1 == l2 && c1 == c2
        then r1
        else r1 <> "-" <> r2

buildErrorExplanation :: Maybe Builder -> ErrorExplanation -> [Builder]
buildErrorExplanation mayDetails (ErrorExplanation reason mayExpected mayActual) =
  join
    [ ["[Reason  ] " <> TB.text reason]
    , maybe [] (\de -> ["[Details ] " <> de]) mayDetails
    , maybe [] (\ex -> ["[Expected] " <> TB.text ex]) mayExpected
    , maybe [] (\ac -> ["[Actual  ] " <> TB.text ac]) mayActual
    ]

buildParseErrorExplanation :: HasLinePos p => ParseErrorExplanation p -> Builder
buildParseErrorExplanation (ParseErrorExplanation sp context mayDetails errExp) =
  let hd =
        join
          [ ["[Pos     ] " <> buildSpan sp]
          , ["[Context ] " <> TB.intercalate " > " (fmap TB.text (toList context)) | not (Seq.null context)]
          ]
      tl = buildErrorExplanation (fmap TB.text mayDetails) errExp
  in  TB.intercalate "\n" (hd ++ tl)

buildAllParseErrorExplanations :: (HasLinePos p, Foldable f) => f (ParseErrorExplanation p) -> Builder
buildAllParseErrorExplanations = TB.intercalate "\n\n" . fmap buildParseErrorExplanation . toList
