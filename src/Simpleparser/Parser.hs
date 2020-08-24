module SimpleParser.Parser
  ( ParserT (..)
  , Parser
  , runParser
  , branchParser
  , suppressParser
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..), ap)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Foldable (toList)
import ListT (ListT (..))
import qualified ListT as ListT
import SimpleParser.Result (ParseResult (..), ParseValue (..))

newtype ParserT e s m a = ParserT { runParserT :: s -> ListT m (ParseResult e s a) }
  deriving (Functor)

type Parser e s a = ParserT e s Identity a

instance Monad m => Applicative (ParserT e s m) where
  pure a = ParserT (\s -> pure (ParseResult (ParseSuccess a) s))
  (<*>) = ap

instance Monad m => Monad (ParserT e s m) where
  return = pure
  parser >>= f = ParserT (\s -> runParserT parser s >>= go) where
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
  throwError e = ParserT (\s -> pure (ParseResult (ParseError e) s))
  catchError parser handler = ParserT (\s -> ListT (go empty (runParserT parser s))) where
    go !acc listt = do
      m <- ListT.uncons listt
      case m of
        Nothing -> pure Nothing
        Just (r@(ParseResult v t), rest) ->
          let newAcc = acc <> rest
          in case v of
            ParseError e -> go newAcc (runParserT (handler e) t)
            ParseSuccess _ -> pure (Just (r, newAcc))

instance Monad m => MonadState s (ParserT e s m) where
  get = ParserT (\s -> pure (ParseResult (ParseSuccess s) s))
  put t = ParserT (const (pure (ParseResult (ParseSuccess ()) t)))

instance MonadTrans (ParserT e s) where
  lift ma = ParserT (\s -> lift (fmap (\a -> ParseResult (ParseSuccess a) s) ma))

runParser :: Parser e s a -> s -> [ParseResult e s a]
runParser m s = runIdentity (ListT.toList (runParserT m s))

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
suppressParser parser = ParserT (\s -> ListT (go [] (runParserT parser s))) where
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
        case v of
          ParseError _ -> filterOk rest
          ParseSuccess _ -> pure (Just (r, ListT (filterOk rest)))
