{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module SimpleParser.Explain where

import Control.Monad (join)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Void (Void, absurd)
import SimpleParser.Chunked (TextualChunked (..))
import SimpleParser.Common (CompoundTextLabel (..), TextLabel (..))
import SimpleParser.Result (CompoundError (..), ParseError (..), RawError (..), StreamError (..),
                            parseErrorEnclosingLabels, parseErrorNarrowestSpan)
import SimpleParser.Stream (LinePos (..), Span (..), Stream (..), TextualStream)
import Text.Builder (Builder)
import qualified Text.Builder as TB

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
  { eeReason :: !Builder
  , eeExpected :: !(Maybe Builder)
  , eeActual :: !(Maybe Builder)
  }

class ExplainError e where
  explainError :: e -> ErrorExplanation

instance ExplainError Void where
  explainError = absurd

endMsg :: Builder
endMsg = "end of stream"

tokB :: Char -> Builder
tokB t = "token '" <> TB.char t <> "'"

mayTokB :: Maybe Char -> Builder
mayTokB = maybe endMsg tokB

chunkB :: TextualChunked chunk => chunk -> Builder
chunkB k = "chunk \"" <> buildChunk k <> "\""

mayChunkB :: TextualChunked chunk => Maybe chunk -> Builder
mayChunkB = maybe endMsg chunkB

instance (Token s ~ Char, TextualChunked (Chunk s)) => ExplainError (StreamError s) where
  explainError (StreamError re) =
    case re of
      RawErrorMatchEnd actTok ->
        ErrorExplanation "failed to match end of stream" (Just endMsg) (Just (tokB actTok))
      RawErrorAnyToken ->
        ErrorExplanation "failed to match any token" (Just "any token") (Just endMsg)
      RawErrorAnyChunk ->
        ErrorExplanation "failed to match any chunk" (Just "any chunk") (Just endMsg)
      RawErrorSatisfyToken mayActTok ->
        ErrorExplanation "failed to satisfy token predicate" Nothing (Just (mayTokB mayActTok))
      RawErrorMatchToken expTok mayActTok ->
        ErrorExplanation "failed to match token" (Just (tokB expTok)) (Just (mayTokB mayActTok))
      RawErrorMatchChunk expChunk mayActChunk ->
        ErrorExplanation "failed to match chunk" (Just (chunkB expChunk)) (Just (mayChunkB mayActChunk))
      RawErrorTakeTokensWhile1 mayActTok ->
        ErrorExplanation "failed to take 1 or more tokens" Nothing (Just (mayTokB mayActTok))
      RawErrorDropTokensWhile1 mayActTok ->
        ErrorExplanation "failed to drop 1 or more tokens" Nothing (Just (mayTokB mayActTok))

instance (Token s ~ Char, TextualChunked (Chunk s), ExplainError e) => ExplainError (CompoundError s e) where
  explainError ce =
    case ce of
      CompoundErrorStream se -> explainError se
      CompoundErrorFail msg -> ErrorExplanation (TB.text msg) Nothing Nothing
      CompoundErrorCustom e -> explainError e

type Explainable l s e = (TextualStream s, ExplainLabel l, ExplainError e)

data ParseErrorExplanation p = ParseErrorExplanation
  { peeSpan :: !(Span p)
  , peeContext :: !(Seq Builder)
  , peeDetails :: !(Maybe Builder)
  , peeErrExp :: !ErrorExplanation
  }

explainParseError :: Explainable l s e => ParseError l s e -> ParseErrorExplanation (Pos s)
explainParseError pe =
  let (mayLab, sp) = parseErrorNarrowestSpan pe
      context = fmap explainLabel (parseErrorEnclosingLabels pe)
      mayDetails = fmap explainLabel mayLab
      errExp = explainError (peError pe)
  in ParseErrorExplanation sp context mayDetails errExp

buildSpan :: Span LinePos -> Builder
buildSpan (Span (LinePos _ sl sc) (LinePos _ el ec)) =
  TB.decimal (succ sl) <> ":" <> TB.decimal (succ sc) <> "-" <> TB.decimal (succ el) <> ":" <> TB.decimal (succ ec)

buildErrorExplanation :: Maybe Builder -> ErrorExplanation -> [Builder]
buildErrorExplanation mayDetails (ErrorExplanation reason mayExpected mayActual) = join
  [ ["[Reason  ] " <> reason]
  , maybe [] (\de -> ["[Details ] " <> de]) mayDetails
  , maybe [] (\ex -> ["[Expected] " <> ex]) mayExpected
  , maybe [] (\ac -> ["[Actual  ] " <> ac]) mayActual
  ]

buildParseErrorExplanation :: ParseErrorExplanation LinePos -> Builder
buildParseErrorExplanation (ParseErrorExplanation sp context mayDetails errExp) =
  let hd = join
        [ ["[Pos       ] " <> buildSpan sp]
        , ["[Context   ] || " <> TB.intercalate " |> " context | not (Seq.null context)]
        ]
      tl = buildErrorExplanation mayDetails errExp
  in TB.intercalate "\n" (hd ++ tl)

buildAllParseErrorExplanations :: Foldable f => f (ParseErrorExplanation LinePos) -> Builder
buildAllParseErrorExplanations = TB.intercalate "\n\n" . fmap buildParseErrorExplanation . toList
