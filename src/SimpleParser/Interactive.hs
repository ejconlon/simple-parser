{-# LANGUAGE OverloadedStrings #-}

module SimpleParser.Interactive
  ( ErrorStyle (..)
  , renderInteractive
  , parseInteractiveStyle
  , parseInteractive
  )
where

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import Errata (Errata (..), prettyErrors)
import Errata.Styles (fancyPointer, fancyStyle)
import SimpleParser.Errata (LinePosExplainable, errataParseError)
import SimpleParser.Explain (Explainable, buildAllParseErrorExplanations, explainParseError)
import SimpleParser.Input (matchEnd)
import SimpleParser.Parser (Parser, runParser)
import SimpleParser.Result (ParseErrorBundle (..), ParseResult (..), ParseSuccess (..))
import SimpleParser.Stream (LinePosStream, newLinePosStream)
import qualified Text.Builder as TB

data ErrorStyle
  = ErrorStyleErrata
  | ErrorStyleExplain
  deriving stock (Eq, Show)

renderInteractive :: (LinePosExplainable l s e) => ErrorStyle -> String -> Maybe (ParseResult l s e a) -> IO ()
renderInteractive errStyle input = \case
  Nothing ->
    putStrLn "No result"
  Just (ParseResultError (ParseErrorBundle es)) ->
    case errStyle of
      ErrorStyleErrata ->
        let blocks = fmap (errataParseError fancyStyle fancyPointer "<interactive>") (toList es)
            errata = Errata Nothing blocks Nothing
            pretty = prettyErrors input [errata]
        in  TLIO.putStrLn pretty
      ErrorStyleExplain ->
        let b = buildAllParseErrorExplanations (fmap explainParseError (toList es))
        in  TIO.putStrLn (TB.run ("Errors:\n" <> b))
  Just (ParseResultSuccess _) ->
    putStrLn "Success"

parseInteractiveStyle :: (s ~ LinePosStream Text, Explainable l s e) => ErrorStyle -> Parser l s e a -> String -> IO (Maybe a)
parseInteractiveStyle errStyle parser input = do
  let mres = runParser (parser <* matchEnd) (newLinePosStream (T.pack input))
  renderInteractive errStyle input mres
  let res = case mres of Just (ParseResultSuccess (ParseSuccess _ a)) -> Just a; _ -> Nothing
  pure res

parseInteractive :: (s ~ LinePosStream Text, Explainable l s e) => Parser l s e a -> String -> IO (Maybe a)
parseInteractive = parseInteractiveStyle ErrorStyleErrata
