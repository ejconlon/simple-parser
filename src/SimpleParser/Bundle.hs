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
import SimpleParser.Labels (emptyLabelStack)
import SimpleParser.Parser (Parser, ParserT (..))
import SimpleParser.Result (ParseResult (..), ParseValue (..))

runBundledParserT :: Monad m => ParserT l s e m a -> s -> m (Either (Seq e) (Maybe a))
runBundledParserT parser = go Empty . runParserT parser emptyLabelStack where
  go !es listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure (Right Nothing)
      Just (ParseResult _ v, nextListt) ->
        case v of
          ParseValueSuccess a -> pure (Right (Just a))
          ParseValueError e -> go (es :|> e) nextListt

runVoidParserT :: Monad m => ParserT l s Void m a -> s -> m (Maybe a)
runVoidParserT parser = fmap go . runBundledParserT parser where
  go e = case e of
    Left _ -> Nothing
    Right ma -> ma

runBundledParser :: Parser l s e a -> s -> Either (Seq e) (Maybe a)
runBundledParser parser = runIdentity . runBundledParserT parser

runVoidParser :: Parser l s Void a -> s -> Maybe a
runVoidParser parser = runIdentity . runVoidParserT parser
