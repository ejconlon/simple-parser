{-# LANGUAGE OverloadedStrings #-}

module SimpleParser.Errata
  ( LinePosExplainable
  , errataParseError
  , errataParseResult
  ) where

import Control.Monad (join)
import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import qualified Data.Text as T
import Errata (Block, PointerStyle, Style, blockMerged')
import SimpleParser.Explain (ErrorExplanation (..), Explainable, ParseErrorExplanation (..), explainParseError)
import SimpleParser.Result (ParseError, ParseErrorBundle (ParseErrorBundle), ParseResult (..))
import SimpleParser.Stream (Col (..), HasLinePos (..), Line (..), Pos, Span (..))

type LinePosExplainable l s e = (Explainable l s e, HasLinePos (Pos s))

errataExplanation :: HasLinePos p => Style -> PointerStyle  -> FilePath -> ParseErrorExplanation p -> Block
errataExplanation bsty psty fp (ParseErrorExplanation sp context mayDetails  (ErrorExplanation reason mayExpected mayActual)) =
  let Span startPos endPos = sp
      startLine = unLine (viewLine startPos)
      startCol = unCol (viewCol startPos)
      endLine = unLine (viewLine endPos)
      endCol = unCol (viewCol endPos)
      mayLabel = Just reason
      mayHeader =
       case context of
         Empty -> Nothing
         _ -> Just (T.intercalate " > " (toList context))
      start = (startLine + 1, startCol + 1, mayLabel)
      end = (endLine + 1, endCol + 1, Nothing)
      mayBody =
        case (mayDetails, mayExpected, mayActual) of
          (Nothing, Nothing, Nothing) -> Nothing
          _ -> Just $ T.unlines $ join
            [ maybe [] (\de -> ["[Details ] " <> de]) mayDetails
            , maybe [] (\ex -> ["[Expected] " <> ex]) mayExpected
            , maybe [] (\ac -> ["[Actual  ] " <> ac]) mayActual
            ]
  in blockMerged' bsty psty fp mayHeader start end mayLabel mayBody

errataParseError :: LinePosExplainable l s e => Style -> PointerStyle -> FilePath -> ParseError l s e -> Block
errataParseError bsty psty fp pe =
  let pee = explainParseError pe
  in errataExplanation bsty psty fp pee

errataParseResult :: LinePosExplainable l s e => Style -> PointerStyle -> FilePath -> ParseResult l s e a -> [Block]
errataParseResult bsty psty fp pr =
  case pr of
    ParseResultError (ParseErrorBundle errs) -> fmap (errataParseError bsty psty fp) (toList errs)
    ParseResultSuccess _ -> []
