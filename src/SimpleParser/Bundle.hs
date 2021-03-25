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

runBundledParserT :: Monad m => ParserT r s e m a -> r -> s -> m (Either (Seq e) (Maybe a))
runBundledParserT parser env = go Empty . runParserT parser env where
  go !es listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure (Right Nothing)
      Just (ParseResult _ v, nextListt) ->
        case v of
          ParseSuccess a -> pure (Right (Just a))
          ParseError e -> go (es :|> e) nextListt

runVoidParserT :: Monad m => ParserT r s Void m a -> r -> s -> m (Maybe a)
runVoidParserT parser env = fmap go . runBundledParserT parser env where
  go e = case e of
    Left _ -> Nothing
    Right ma -> ma

runBundledParser :: Parser r s e a -> r -> s -> Either (Seq e) (Maybe a)
runBundledParser parser env = runIdentity . runBundledParserT parser env

runVoidParser :: Parser r s Void a -> r -> s -> Maybe a
runVoidParser parser env = runIdentity . runVoidParserT parser env
