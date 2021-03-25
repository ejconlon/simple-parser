module SimpleParser.Experimental.Report
  ( ReportError (..)
  , ParserWithReport
  , ContextError (..)
  , throwContextError
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (Reader, asks)
import SimpleParser.Common (getStreamPos)
import SimpleParser.Experimental.Assert (EmbedStreamAssertError (..), EnrichStreamAssertError (..), MatchStreamAssertError (..))
import SimpleParser.Experimental.Context (ContextStack, HasContextStack (..), askContextStack)
import SimpleParser.Parser (ParserT, mapErrorParser)
import SimpleParser.Stream (Span (..), StreamWithPos (..))

-- | An error with context
data ContextError c e = ContextError
  { ceContext :: !(ContextStack c)
  , ceError :: !e
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasContextStack c (ContextError c e) where
  viewContextStack = ceContext
  setContextStack x e = e { ceContext = x }

instance MatchStreamAssertError s e => MatchStreamAssertError s (ContextError c e) where
  matchStreamAssertError = matchStreamAssertError . ceError

instance (EmbedStreamAssertError s e, HasContextStack c r) => EnrichStreamAssertError s (ContextError c e) (Reader r) where
  enrichStreamAssertError e = asks (\env -> ContextError (viewContextStack env) (embedStreamAssertError e))

-- | Throws an error with context.
throwContextError :: (HasContextStack c r, Monad m) => e -> ParserT r s (ContextError c e) m a
throwContextError e = do
  ctx <- askContextStack
  throwError (ContextError ctx e)

data ReportError c p e = ReportError
  { reContext :: !(ContextStack c)
  , reSpan :: !(Span p)
  , reError :: !e
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasContextStack c (ReportError c p e) where
  viewContextStack = reContext
  setContextStack x e = e { reContext = x }

type ParserWithReport c p r s m = (StreamWithPos p s, HasContextStack c r, Monad m)

-- | Adds context spans to all errors in the given parser.
reportSpanParser :: (ParserWithReport c p r s m) => ParserT r s (ContextError c e) m a -> ParserT r s (ReportError c p e) m a
reportSpanParser p = do
  start <- getStreamPos
  mapErrorParser (\s (ContextError ctx e) -> ReportError ctx (Span start (viewStreamPos s)) e) p
