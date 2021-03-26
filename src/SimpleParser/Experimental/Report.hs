-- {-# LANGUAGE UndecidableInstances #-}

module SimpleParser.Experimental.Report where
--   ( ReportError (..)
--   , ParserWithReport
--   , ContextError (..)
--   , throwContextError
--   ) where

-- import Control.Monad.Except (throwError)
-- import SimpleParser.Common (getStreamPos)
-- import SimpleParser.Experimental.Assert (EmbedStreamAssertError (..), EnrichStreamAssertError (..),
--                                          MatchStreamAssertError (..))
-- import SimpleParser.Experimental.Context (ContextStack, HasContextStack (..), askContextStack)
-- import SimpleParser.Parser (ParserT, mapErrorParser)
-- import SimpleParser.Stream (Span (..), StreamWithPos (..))

-- -- | An error with context
-- data ContextError c e = ContextError
--   { ceContext :: !(ContextStack c)
--   , ceError :: !e
--   } deriving (Eq, Show, Functor, Foldable, Traversable)

-- instance HasContextStack c (ContextError c e) where
--   viewContextStack = ceContext
--   setContextStack x e = e { ceContext = x }

-- -- | Throws an error with context.
-- throwContextError :: (HasContextStack c r, Monad m) => e -> ParserT r s (ContextError c e) m a
-- throwContextError e = do
--   ctx <- askContextStack
--   throwError (ContextError ctx e)

-- -- | Throwing 'StreamAssertError' requires that we index what labels we can throw.
-- -- You'll need a `type instance ContextLabel ... = ...` with your parser to do so.
-- type family ContextLabel c s e

-- instance (l ~ ContextLabel c s e, MatchStreamAssertError l s e) => MatchStreamAssertError l s (ContextError c e) where
--   matchStreamAssertError = matchStreamAssertError . ceError

-- instance (l ~ ContextLabel c s e, EmbedStreamAssertError l s e, HasContextStack c r) => EnrichStreamAssertError l r s (ContextError c e) where
--   enrichStreamAssertError env e = ContextError (viewContextStack env) (embedStreamAssertError e)

-- -- | An error with context and span
-- data ReportError c p e = ReportError
--   { reContext :: !(ContextStack c)
--   , reSpan :: !(Span p)
--   , reError :: !e
--   } deriving (Eq, Show, Functor, Foldable, Traversable)

-- instance HasContextStack c (ReportError c p e) where
--   viewContextStack = reContext
--   setContextStack x e = e { reContext = x }

-- type ParserWithReport c p r s m = (StreamWithPos p s, HasContextStack c r, Monad m)

-- -- | Adds context spans to all errors in the given parser.
-- reportSpanParser :: (ParserWithReport c p r s m) => ParserT r s (ContextError c e) m a -> ParserT r s (ReportError c p e) m a
-- reportSpanParser p = do
--   start <- getStreamPos
--   mapErrorParser (\s (ContextError ctx e) -> ReportError ctx (Span start (viewStreamPos s)) e) p
