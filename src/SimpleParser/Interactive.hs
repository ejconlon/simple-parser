{-# LANGUAGE OverloadedStrings #-}

module SimpleParser.Interactive
  ( parseInteractive
  ) where

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import SimpleParser.Explain (Explainable, buildAllParseErrorExplanations, explainParseError)
import SimpleParser.Input (matchEnd)
import SimpleParser.Labels (emptyLabelStack)
import SimpleParser.Parser (Parser, runParser)
import SimpleParser.Result (ParseResult (..), ParseSuccess (..))
import SimpleParser.Stream (LinePosStream, newLinePosStream)
import qualified Text.Builder as TB

parseInteractive :: (s ~ LinePosStream Text, Explainable l s e, Show a) => Parser l s e a -> String -> IO ()
parseInteractive parser input =
  case runParser (parser <* matchEnd) emptyLabelStack (newLinePosStream (T.pack input)) of
    Nothing ->
      putStrLn "No result."
    Just (ParseResultError es) ->
      let b = buildAllParseErrorExplanations (fmap explainParseError (toList es))
      in TIO.putStrLn (TB.run ("Errors:\n" <> b))
    Just (ParseResultSuccess (ParseSuccess _ a)) ->
      putStrLn "Success:" *> print a
