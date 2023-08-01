{-# language TemplateHaskell #-}

module SaferC.Check where

import SaferC.Types

import Data.Foldable (traverse_)
import Control.Lens
import Data.Map (Map)
import Data.Map qualified as Map
import Control.Monad (unless)
import Control.Comonad (extract)
import Control.Comonad.Cofree (unwrap)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT, local, runReaderT)

data Env = Env
  { _envResultType :: Maybe Type
  , _envNamedTypes :: Map Identifier (Sourced (Maybe Type))
  , _envBindings :: Map Identifier (Sourced Type)
  }

makeLenses ''Env

emptyEnv :: Env
emptyEnv = Env
  { _envResultType = Nothing
  , _envNamedTypes = Map.empty
  , _envBindings = Map.empty
  }

type Check a = ReaderT Env (Either (Sourced Error)) a

data Error
  = TypeError { typeErrorExpected :: String , typeErrorFound :: Type }
  | ShadowError (Sourced Identifier)
  deriving (Show)

typeError :: ExprLoc -> String -> Type -> Check a
typeError expr expected found = throwError $ Sourced (extract expr) $ TypeError expected found

shadowError :: Identifier -> Source -> Source -> Check a
shadowError name original new = throwError $ Sourced new $ ShadowError $ Sourced original name

expectType :: ExprLoc -> Type -> Check ()
expectType expr expected = do
  found <- checkExpr expr
  unless (expected <: found) $ typeError expr (show expected) found

expectIndex :: ExprLoc -> Check ()
expectIndex x = checkExpr x >>= \case
  Byte -> pure ()
  Size -> pure ()
  tx -> typeError x "unsigned integer" tx

expectArray :: ExprLoc -> Check Type
expectArray a = checkExpr a >>= go
  where
    go :: Type -> Check Type
    go (ArrayOf _ tx) = pure tx
    go (OwnedPointerTo _ t) = go t
    go ta = typeError a "array" ta

expectComparable :: ExprLoc -> ExprLoc -> Check Type
expectComparable x y = do
    tx <- checkExpr x
    ty <- checkExpr y
    case tx of
      LiteralT _
        | ty <: tx -> pure ()
        | otherwise -> typeError x (show ty) tx
      _ -> expectType y tx
    pure Bool

checkExpr :: ExprLoc -> Check Type
checkExpr expr = case unwrap expr of
  Literal lit -> pure (LiteralT lit)
  Equal x y -> expectComparable x y
  Unequal x y -> expectComparable x y
  LessThan x y -> expectComparable x y
  LessOrEqual x y -> expectComparable x y
  And x y -> expectType x Bool >> expectType y Bool >> pure Bool
  Or x y -> expectType x Bool >> expectType y Bool >> pure Bool
  OrElse x y -> do
    tx <- checkExpr x
    ty <- checkExpr y
    case tx of
      Fallible tx'
        | tx' <: ty -> pure tx'
        | otherwise -> typeError y (show tx') ty
      _ -> typeError x "fallible operation" tx
  Not x -> expectType x Bool >> pure Bool
  AddressOf x -> do
    tx <- checkExpr x
    pure (OwnedPointerTo undefined tx)
  Deref x -> do
    tx <- checkExpr x
    case tx of
      OwnedPointerTo _ tx' -> pure tx'
      _ -> typeError x "pointer" tx
  Index a i -> do
    expectIndex i
    expectArray a
  Variable _ -> undefined
  Access _ _ -> undefined
  Return x -> do
    pure NoReturn
  Call _ _ -> undefined

withStatement :: Statement -> Check a -> Check a
withStatement (Let (Sourced src name) typ value) f = do
  expectType value typ
  view (envBindings . at name)
    >>= traverse_ (\(Sourced oldSrc _) -> shadowError name oldSrc src)
  local (envBindings . at name ?~ Sourced src typ) f

withStatement _ f = f

withDefinition :: Definition -> Check a -> Check a
withDefinition (TopComments _) f = f

withDefinition (TypeDef (Sourced newSrc name) mbType) f = do
  mbExisting <- view $ envNamedTypes . at name
  case (mbExisting, mbType) of
    (Just (Sourced oldSrc (Just existing)), Just new) 
      | existing /= new -> shadowError name oldSrc newSrc
    (Nothing, _) -> local (envNamedTypes . at name ?~ Sourced newSrc mbType) f
    _ -> f

withDefinition (GlobalDef (Sourced newSrc name) typ _) f = do
  mbExisting <- view $ envBindings . at name
  case mbExisting of
    Just (Sourced oldSrc _) -> shadowError name oldSrc newSrc
    Nothing -> local (envBindings . at name ?~ Sourced newSrc typ) f

withDefinition (FunctionDef _ (Sourced newSrc name) params ret _) f = do
  mbExisting <- view $ envBindings . at name
  case mbExisting of
    Just (Sourced oldSrc _) -> shadowError name oldSrc newSrc
    Nothing -> local (envBindings . at name ?~ Sourced newSrc typ) f
  where
    typ = FunctionOf (paramType <$> params) ret

check :: [Definition] -> Either [Sourced Error] ()
check defs = _Left %~ pure $
  flip runReaderT emptyEnv $
  flip (foldr withDefinition) defs $
  pure ()
