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
import SimpleParser.Labels (LabelStack (..))
import SimpleParser.Result (AnchoredParseError (..), CompoundError (..), ParseError (..), RawError (..),
                            StreamError (..))
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
  , eeActual :: !Builder
  }

class ExplainError e where
  explainError :: e -> ErrorExplanation

instance ExplainError Void where
  explainError = absurd

tokB :: Char -> Builder
tokB t = "token '" <> TB.char t <> "'"

mayTokB :: Maybe Char -> Builder
mayTokB = maybe "end of stream" tokB

chunkB :: TextualChunked chunk => chunk -> Builder
chunkB k = "chunk \"" <> buildChunk k <> "\""

mayChunkB :: TextualChunked chunk => Maybe chunk -> Builder
mayChunkB = maybe "end of stream" chunkB

instance (Token s ~ Char, TextualChunked (Chunk s), ExplainLabel l) => ExplainError (StreamError l s) where
  explainError (StreamError re) =
    case re of
      RawErrorMatchEnd actTok ->
        ErrorExplanation "failed to match end of stream" (Just "end of stream") (tokB actTok)
      RawErrorAnyToken ->
        ErrorExplanation "failed to match any token" (Just "any token") "end of stream"
      RawErrorAnyChunk ->
        ErrorExplanation "failed to match any chunk" (Just "any chunk") "end of stream"
      RawErrorSatisfyToken mayLab mayActTok ->
        ErrorExplanation "failed to satisfy token predicate" (fmap explainLabel mayLab) (mayTokB mayActTok)
      RawErrorMatchToken expTok mayActTok ->
        ErrorExplanation "failed to match token" (Just (tokB expTok)) (mayTokB mayActTok)
      RawErrorMatchChunk expChunk mayActChunk ->
        ErrorExplanation "failed to match chunk" (Just (chunkB expChunk)) (mayChunkB mayActChunk)
      RawErrorTakeTokensWhile1 mayLab mayActTok ->
        ErrorExplanation "failed to take 1 or more tokens" (fmap explainLabel mayLab) (mayTokB mayActTok)
      RawErrorDropTokensWhile1 mayLab mayActTok ->
        ErrorExplanation "failed to drop 1 or more tokens" (fmap explainLabel mayLab) (mayTokB mayActTok)

instance (Token s ~ Char, TextualChunked (Chunk s), ExplainLabel l, ExplainError e) => ExplainError (CompoundError l s e) where
  explainError ce =
    case ce of
      CompoundErrorStream se -> explainError se
      CompoundErrorCustom e -> explainError e

type Explainable l s e = (TextualStream s, ExplainLabel l, ExplainError e)

data ParseErrorExplanation p = ParseErrorExplanation
  { peeSpan :: !(Span p)
  , peeLabExps :: !(Seq Builder)
  , peeErrExp :: !ErrorExplanation
  }

explainParseError :: Explainable l s e => AnchoredParseError l s e -> ParseErrorExplanation (Pos s)
explainParseError (AnchoredParseError startState (ParseError labelStack endState err)) =
  let startPos = streamViewPos startState
      endPos = streamViewPos endState
      sp = Span startPos endPos
      labExps = fmap explainLabel (unLabelStack labelStack)
      errExp = explainError err
  in ParseErrorExplanation sp labExps errExp

buildSpan :: Span LinePos -> Builder
buildSpan (Span (LinePos _ sl sc) (LinePos _ el ec)) =
  TB.decimal (succ sl) <> ":" <> TB.decimal (succ sc) <> "-" <> TB.decimal (succ el) <> ":" <> TB.decimal (succ ec)

buildParseErrorExplanation :: ParseErrorExplanation LinePos -> Builder
buildParseErrorExplanation (ParseErrorExplanation sp labExps (ErrorExplanation reason mayExpected actual)) =
  TB.intercalate "\n" $ join
    [ ["[Pos     ] " <> buildSpan sp]
    , ["[Stack   ] || " <> TB.intercalate " |> " labExps | not (Seq.null labExps)]
    , ["[Reason  ] " <> reason]
    , maybe [] (\ex -> ["[Expected] " <> ex]) mayExpected
    , ["[Actual  ] " <> actual]
    ]

buildAllParseErrorExplanations :: Foldable f => f (ParseErrorExplanation LinePos) -> Builder
buildAllParseErrorExplanations = TB.intercalate "\n" . fmap buildParseErrorExplanation . toList
