{-# LANGUAGE OverloadedStrings #-}

-- | Parses simple Sexp-formatted logical propositions
module SimpleParser.Examples.Prop where

import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Void (Void)
import SimpleParser.Examples.Ast (AstLabel (..), AstParserC, AstParserM, Ctor (..), CtorDefns, astParser,
                                  identAstParser)

type PropParserC s = AstParserC s
type PropParserM s a = AstParserM s Void a

data SProp v =
    SPropVar !v
  | SPropBool !Bool
  | SPropNot (SProp v)
  | SPropAnd !(Seq (SProp v))
  | SPropOr !(Seq (SProp v))
  | SPropIf !(Seq (SProp v)) (SProp v)
  | SPropIff (SProp v) (SProp v)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

guard2 :: MonadFail m => Seq a -> m (Seq a)
guard2 ss = if Seq.length ss < 2 then fail "must have at least two subterms" else pure ss

guardLast :: MonadFail m => Seq a -> m (Seq a, a)
guardLast ss = case ss of { xs :|> x | not (Seq.null xs) -> pure (xs, x); _ -> fail "must have at least two subterms" }

mkPropCtors :: PropParserM s (SProp Text) -> CtorDefns s Void (SProp Text)
mkPropCtors root = Map.fromList
  [ ("<=>", Ctor2 (\a b -> pure (SPropIff a b)) root root)
  , ("not", Ctor1 (pure . SPropNot) root)
  , ("and", CtorN (fmap SPropAnd . guard2) root)
  , ("or", CtorN (fmap SPropOr . guard2) root)
  , ("=>", CtorN (fmap (uncurry SPropIf) . guardLast) root)
  ]

mkPropAtom :: PropParserC s => PropParserM s (SProp Text)
mkPropAtom = do
  ident <- identAstParser (Just (AstLabelCustom "atom"))
  case ident of
    "true" -> pure (SPropBool True)
    "false" -> pure (SPropBool False)
    _ -> pure (SPropVar ident)

propParser :: PropParserC s => PropParserM s (SProp Text)
propParser = astParser mkPropAtom mkPropCtors
