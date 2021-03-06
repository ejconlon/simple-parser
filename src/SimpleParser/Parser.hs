module SimpleParser.Parser
  ( ParserT (..)
  , Parser
  , runParser
  , filterParser
  , reflectParser
  , branchParser
  , suppressParser
  , defaultParser
  , optionalParser
  , silenceParser
  , greedyStarParser
  , greedyStarParser_
  , greedyPlusParser
  , greedyPlusParser_
  ) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad (MonadPlus (..), ap, (>=>))
import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Foldable (toList)
import ListT (ListT (..))
import qualified ListT
import SimpleParser.Result (ParseResult (..), ParseValue (..))

newtype ParserT e s m a = ParserT { runParserT :: s -> ListT m (ParseResult e s a) }
  deriving (Functor)

type Parser e s a = ParserT e s Identity a

instance Monad m => Applicative (ParserT e s m) where
  pure a = ParserT (pure . ParseResult (ParseSuccess a))
  (<*>) = ap

instance Monad m => Monad (ParserT e s m) where
  return = pure
  parser >>= f = ParserT (runParserT parser >=> go) where
    go (ParseResult v t) =
      case v of
        ParseError e -> pure (ParseResult (ParseError e) t)
        ParseSuccess a -> runParserT (f a) t

instance Monad m => Alternative (ParserT e s m) where
  empty = ParserT (const empty)
  first <|> second = ParserT (\s -> runParserT first s <|> runParserT second s)

instance Monad m => MonadPlus (ParserT e s m) where
  mzero = empty
  mplus = (<|>)

instance Monad m => MonadError e (ParserT e s m) where
  throwError e = ParserT (pure . ParseResult (ParseError e))
  -- TODO implement directly by unwrapping?
  catchError parser handler = do
    r <- reflectParser parser
    case r of
      ParseError e -> handler e
      ParseSuccess a -> pure a

instance Monad m => MonadState s (ParserT e s m) where
  get = ParserT (\s -> pure (ParseResult (ParseSuccess s) s))
  put t = ParserT (const (pure (ParseResult (ParseSuccess ()) t)))
  state f = ParserT (\s -> let (a, t) = f s in pure (ParseResult (ParseSuccess a) t))

instance MonadTrans (ParserT e s) where
  lift ma = ParserT (\s -> lift (fmap (\a -> ParseResult (ParseSuccess a) s) ma))

runParser :: Parser e s a -> s -> [ParseResult e s a]
runParser m s = runIdentity (ListT.toList (runParserT m s))

filterParser :: Monad m => (a -> Bool) -> ParserT e s m a -> ParserT e s m a
filterParser f parser = ParserT (ListT . go . runParserT parser) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (r@(ParseResult v _), rest) ->
        case v of
          ParseSuccess a | not (f a) -> go rest
          _ -> pure (Just (r, ListT (go rest)))

reflectParser :: Monad m => ParserT e s m a -> ParserT e s m (ParseValue e a)
reflectParser parser = ParserT (ListT . go . runParserT parser) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (ParseResult v t, rest) ->
        pure (Just (ParseResult (ParseSuccess v) t, ListT (go rest)))

branchParser :: (Foldable f, Monad m) => f (ParserT e s m a) -> ParserT e s m a
branchParser = start . toList where
  start ps =
    case ps of
      [] -> empty
      q:qs -> ParserT (\s -> ListT (run s q qs))
  run s q qs = do
    m <- ListT.uncons (runParserT q s)
    case m of
      Nothing ->
        case qs of
          [] -> pure Nothing
          r:rs -> run s r rs
      Just (a, rest) -> pure (Just (a, rest))

suppressParser :: Monad m => ParserT e s m a -> ParserT e s m a
suppressParser parser = ParserT (ListT . go [] . runParserT parser) where
  go !acc listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> returnErr (reverse acc)
      Just (r@(ParseResult v _), rest) ->
        case v of
          ParseError _ -> go (r:acc) rest
          ParseSuccess _ -> pure (Just (r, ListT (filterOk rest)))

  returnErr racc =
    case racc of
      [] -> pure Nothing
      r:rs -> pure (Just (r, ListT (returnErr rs)))

  filterOk listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (r@(ParseResult v _), rest) ->
        let nextListt = filterOk rest
        in case v of
          ParseError _ -> nextListt
          ParseSuccess _ -> pure (Just (r, ListT nextListt))

defaultParser :: Monad m => a -> ParserT e s m a -> ParserT e s m a
defaultParser def parser = ParserT (\s -> ListT (go s (runParserT parser s))) where
  go s listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure (Just (ParseResult (ParseSuccess def) s, empty))
      Just _ -> pure m

optionalParser :: Monad m => ParserT e s m a -> ParserT e s m (Maybe a)
optionalParser parser = defaultParser Nothing (fmap Just parser)

silenceParser :: Monad m => ParserT e s m a -> ParserT e s m a
silenceParser parser = ParserT (ListT . go . runParserT parser) where
  go listt = do
    m <- ListT.uncons listt
    case m of
      Nothing -> pure Nothing
      Just (r@(ParseResult v _), rest) ->
        let nextListt = go rest
        in case v of
          ParseError _ -> nextListt
          ParseSuccess _ -> pure (Just (r, ListT nextListt))

greedyStarParser :: Monad m => ParserT e s m a -> ParserT e s m [a]
greedyStarParser parser = go [] where
  opt = optionalParser parser
  go !acc = do
    res <- opt
    case res of
      Nothing -> pure (reverse acc)
      Just a -> go (a:acc)

greedyStarParser_ :: Monad m => ParserT e s m a -> ParserT e s m ()
greedyStarParser_ parser = go where
  opt = optionalParser parser
  go = do
    res <- opt
    case res of
      Nothing -> pure ()
      Just _ -> go

greedyPlusParser :: Monad m => ParserT e s m a -> ParserT e s m [a]
greedyPlusParser parser = liftA2 (:) parser (greedyStarParser parser)

greedyPlusParser_ :: Monad m => ParserT e s m a -> ParserT e s m ()
greedyPlusParser_ parser = parser *> greedyStarParser_ parser
