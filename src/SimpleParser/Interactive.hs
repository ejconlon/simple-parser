{-# LANGUAGE OverloadedStrings #-}

module SimpleParser.Interactive
  ( ErrorStyle (..)
  , parseInteractiveStyle
  , parseInteractive
  ) where

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import Errata (Errata (..), fancyStyle, prettyErrors)
import SimpleParser.Errata (errataParseError)
import SimpleParser.Explain (Explainable, buildAllParseErrorExplanations, explainParseError)
import SimpleParser.Input (matchEnd)
import SimpleParser.Parser (Parser, runParser)
import SimpleParser.Result (ParseResult (..), ParseSuccess (..))
import SimpleParser.Stream (LinePosStream, newLinePosStream)
import qualified Text.Builder as TB

data ErrorStyle =
    ErrorStyleErrata
  | ErrorStyleExplain
  deriving stock (Eq, Show)

parseInteractiveStyle :: (s ~ LinePosStream Text, Explainable l s e, Show a) => ErrorStyle -> Parser l s e a -> String -> IO ()
parseInteractiveStyle errStyle parser input =
  case runParser (parser <* matchEnd) (newLinePosStream (T.pack input)) of
    Nothing ->
      putStrLn "No result."
    Just (ParseResultError es) ->
      case errStyle of
        ErrorStyleErrata ->
          let blocks = fmap (errataParseError fancyStyle "<interactive>") (toList es)
              errata = Errata Nothing blocks Nothing
              pretty = prettyErrors input [errata]
          in TLIO.putStrLn pretty
        ErrorStyleExplain ->
          let b = buildAllParseErrorExplanations (fmap explainParseError (toList es))
          in TIO.putStrLn (TB.run ("Errors:\n" <> b))
    Just (ParseResultSuccess (ParseSuccess _ a)) ->
      putStrLn "Success:" *> print a

parseInteractive :: (s ~ LinePosStream Text, Explainable l s e, Show a) => Parser l s e a -> String -> IO ()
parseInteractive = parseInteractiveStyle ErrorStyleErrata
