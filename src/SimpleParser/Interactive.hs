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
import SimpleParser.Parser (Parser)
import SimpleParser.Perform (performParser)
import SimpleParser.Stream (LinePosStream, newLinePosStream)
import qualified Text.Builder as TB

parseInteractive :: (s ~ LinePosStream Text, Explainable l s e, Show a) => Parser l s e a -> String -> IO ()
parseInteractive parser input =
  case performParser (parser <* matchEnd) (newLinePosStream (T.pack input)) of
    Left es ->
      let b = buildAllParseErrorExplanations (fmap explainParseError (toList es))
      in TIO.putStrLn (TB.run ("Error:\n" <> b))
    Right ma ->
      case ma of
        Nothing -> putStrLn "No result."
        Just a -> putStrLn "Success:" *> print a
