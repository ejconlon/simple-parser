module SimpleParser.Perform
  ( performParserT
  , performParser
  ) where

import Control.Monad.Identity (Identity (..))
import Data.Sequence (Seq (..))
import qualified ListT
import SimpleParser.Labels (emptyLabelStack)
import SimpleParser.Parser (Parser, ParserT (..))
import SimpleParser.Result (AnchoredParseError (..), ParseResult (..), ParseValue (..))

performParserT :: Monad m => ParserT l s e m a -> s -> m (Either (Seq (AnchoredParseError l s e)) (Maybe a))
performParserT parser = go Empty . runParserT parser emptyLabelStack where
  go !es listt = do
    m <- ListT.uncons listt
    case m of
      Nothing ->
        case es of
          Empty -> pure (Right Nothing)
          _ -> pure (Left es)
      Just (ParseResult s v, nextListt) ->
        case v of
          ParseValueSuccess a -> pure (Right (Just a))
          ParseValueError e -> go (es :|> AnchoredParseError s e) nextListt

performParser :: Parser l s e a -> s -> Either (Seq (AnchoredParseError l s e)) (Maybe a)
performParser parser = runIdentity . performParserT parser
