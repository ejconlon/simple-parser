module SimpleParser.Throw
  ( EmptyParseError (..)
  , runParserThrow
  , runParserEnd
  )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Typeable (Typeable)
import SimpleParser.Input (matchEnd)
import SimpleParser.Parser (Parser, runParser)
import SimpleParser.Result (ParseResult (..), ParseSuccess (..))
import SimpleParser.Stream (Chunk, Stream, Token)

data EmptyParseError = EmptyParseError
  deriving stock (Eq, Show)

instance Exception EmptyParseError

-- | Runs a parser and throws bundled errors / no parse result errors as exceptions.
runParserThrow
  :: ( Typeable l
     , Typeable s
     , Typeable e
     , Typeable (Token s)
     , Typeable (Chunk s)
     , Show l
     , Show s
     , Show e
     , Show (Token s)
     , Show (Chunk s)
     , MonadThrow m
     )
  => Parser l s e a
  -> s
  -> m (ParseSuccess s a)
runParserThrow parser s =
  case runParser parser s of
    Nothing -> throwM EmptyParseError
    Just res ->
      case res of
        ParseResultError errs -> throwM errs
        ParseResultSuccess success -> pure success

-- | The easiest way to fully consume input and throw errors.
runParserEnd
  :: ( Typeable l
     , Typeable s
     , Typeable e
     , Typeable (Token s)
     , Typeable (Chunk s)
     , Show l
     , Show s
     , Show e
     , Show (Token s)
     , Show (Chunk s)
     , Stream s
     , MonadThrow m
     )
  => Parser l s e a
  -> s
  -> m a
runParserEnd parser s = fmap psValue (runParserThrow (parser <* matchEnd) s)
