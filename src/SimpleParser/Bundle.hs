module SimpleParser.Bundle
  ( runBundledParserT
  , runBundledParser
  ) where

import Control.Monad.Identity (Identity (..))
import Data.Sequence (Seq (..))
import qualified ListT
import SimpleParser.Labels (emptyLabelStack)
import SimpleParser.Parser (Parser, ParserT (..))
import SimpleParser.Result (ParseError, ParseResult (..), ParseValue (..))

runBundledParserT :: Monad m => ParserT l s e m a -> s -> m (Either (Seq (ParseError l s e)) (Maybe a))
runBundledParserT parser = go Empty . runParserT parser emptyLabelStack where
  go !es listt = do
    m <- ListT.uncons listt
    case m of
      Nothing ->
        case es of
          Empty -> pure (Right Nothing)
          _ -> pure (Left es)
      Just (ParseResult _ v, nextListt) ->
        case v of
          ParseValueSuccess a -> pure (Right (Just a))
          ParseValueError e -> go (es :|> e) nextListt

runBundledParser :: Parser l s e a -> s -> Either (Seq (ParseError l s e)) (Maybe a)
runBundledParser parser = runIdentity . runBundledParserT parser
