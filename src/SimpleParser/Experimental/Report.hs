{-# LANGUAGE UndecidableInstances #-}

module SimpleParser.Experimental.Report
  ( ReportError (..)
  , ParserWithReport
  , ContextError (..)
  , throwContextError
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import SimpleParser.Common (getStreamPos)
import SimpleParser.Experimental.Assert (EmbedStreamAssertError (..), EnrichStreamAssertError (..), MatchStreamAssertError (..))
import SimpleParser.Experimental.Context (ContextStack, HasContextStack (..), MonadContext, askContextStack)
import SimpleParser.Parser (ParserT, mapErrorStateParser)
import SimpleParser.Stream (Span (..), StreamWithPos (..))

data ContextError c e = ContextError
  { ceContext :: !(ContextStack c)
  , ceError :: !e
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasContextStack c (ContextError c e) where
  viewContextStack = ceContext
  setContextStack x e = e { ceContext = x }

instance MatchStreamAssertError e s => MatchStreamAssertError (ContextError c e) s where
  matchStreamAssertError = matchStreamAssertError . ceError

instance (EmbedStreamAssertError e s, MonadContext c r m) => EnrichStreamAssertError (ContextError c e) s m where
  enrichStreamAssertError e = asks (\env -> ContextError (viewContextStack env) (embedStreamAssertError e))

-- | Throws an error with context.
throwContextError :: MonadContext c r m => e -> ParserT (ContextError c e) s m a
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

type ParserWithReport c r p s m = (StreamWithPos p s, MonadContext c r m)

-- | Adds context spans to all errors in the given parser.
reportSpanParser :: (ParserWithReport c r p s m) => ParserT (ContextError c e) s m a -> ParserT (ReportError c p e) s m a
reportSpanParser p = do
  start <- getStreamPos
  mapErrorStateParser (\(ContextError ctx e) s -> ReportError ctx (Span start (viewStreamPos s)) e) p
