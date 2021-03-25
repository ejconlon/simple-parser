module SimpleParser.Bundle
  ( runBundledParserT
  , runVoidParserT
  , runBundledParser
  , runVoidParser
  ) where

import Control.Monad.Identity (Identity (..))
import Data.Sequence (Seq (..))
import Data.Void (Void)
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

runVoidParserT :: Monad m => ParserT Void s m a -> s -> m (Maybe a)
runVoidParserT parser = fmap go . runBundledParserT parser where
  go e = case e of
    Left _ -> Nothing
    Right ma -> ma

runBundledParser :: Parser e s a -> s -> Either (Seq e) (Maybe a)
runBundledParser parser = runIdentity . runBundledParserT parser

runVoidParser :: Parser Void s a -> s -> Maybe a
runVoidParser parser = runIdentity . runVoidParserT parser
