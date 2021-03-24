module SimpleParser.Bundle
  ( runBundledParserT
  , runBundledParser
  ) where

import Control.Monad.Identity (Identity (..))
import Data.Sequence (Seq (..))
import qualified ListT
import SimpleParser.Parser (Parser, ParserT (..))
import SimpleParser.Result (ParseResult (..), ParseValue (..))

runBundledParserT :: Monad m => ParserT e s m a -> s -> m (Either (Seq e) (Maybe a))
runBundledParserT parser = go Empty . runParserT parser where
  go !es listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure (Right Nothing)
      Just (ParseResult v _, nextListt) ->
        case v of
          ParseSuccess a -> pure (Right (Just a))
          ParseError e -> go (es :|> e) nextListt

runBundledParser :: Parser e s a -> s -> Either (Seq e) (Maybe a)
runBundledParser parser = runIdentity . runBundledParserT parser
