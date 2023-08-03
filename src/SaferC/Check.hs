{-# language TemplateHaskell #-}

module SaferC.Check where

import Data.Foldable (traverse_)
import Control.Lens
import Data.Map (Map)
import Data.Map qualified as Map
import Control.Monad (unless)
import Control.Comonad (extract)
import Control.Comonad.Cofree (unwrap)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT, local, runReaderT)

import SaferC.Error
import SaferC.Types

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

expectType :: ExprLoc -> Type -> Check ()
expectType expr expected = do
  found <- checkExpr expr
  unless (expected <: found) $ typeError expr (show expected) found

expectIndex :: ExprLoc -> Check ()
expectIndex x = checkExpr x >>= \case
  Byte -> pure ()
  Size -> pure ()
  LiteralT (Integer n) | n >= 0 -> pure ()
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

checkCast :: Source -> Type -> Type -> Check Type
checkCast src castTo castFrom = case (castTo, castFrom) of
  (Byte, Int) -> pure $ Fallible Byte
  (Byte, Size) -> pure $ Fallible Byte
  (Int, Byte) -> pure Int
  (Int, Size) -> pure $ Fallible Int
  (Size, Byte) -> pure Size
  (Size, Int) -> pure $ Fallible Size
  (x, y) | x == y -> pure x
  _ -> throwError $ Sourced src $ TypeError expected castFrom
  where expected = "type compatible with " <> show castTo

checkExpr :: ExprLoc -> Check Type
checkExpr expr = case unwrap expr of
  Literal lit -> pure (LiteralT lit)
  Equal x y -> expectComparable x y
  Unequal x y -> expectComparable x y
  LessThan x y -> expectComparable x y
  LessOrEqual x y -> expectComparable x y
  And x y -> expectType x Bool >> expectType y Bool >> pure Bool
  Or x y -> do
    tx <- checkExpr x
    unless (Bool <: tx) $ typeError x "Bool" tx
    expectType y tx
    pure tx
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
    Fallible <$> expectArray a
  Variable name -> view (envBindings . at name) >>= \case
    Just typ -> pure $ unSourced typ
    Nothing -> throwError $ Sourced (extract expr) $ NotDefined name
  Access _ _ -> undefined
  Return mbResult -> do
    view envResultType >>= \case
      Nothing -> throwError $ Sourced (extract expr) ReturnOutsideFunction
      Just resultType -> case (mbResult, resultType) of
        (Nothing, Inert) -> pure ()
        (Nothing, _) -> throwError $ Sourced (extract expr) MissingReturnValue
        (Just result, _) -> expectType result resultType
    pure NoReturn
  Call fn args -> do
    checkExpr fn >>= \case
      (FunctionOf params ret) -> do
        checkArgCount (length params) args
        traverse_ (uncurry expectType) $ zip args params
        pure ret
      (CastTo castTo) -> case args of
        [fromExpr] -> do
          castFrom <- checkExpr fromExpr
          checkCast (extract fromExpr) castTo castFrom
        _ -> throwError $ Sourced (foldMap extract args) $ WrongArgCount 1 (length args)
      fnType -> typeError fn "function" fnType

withStatement :: Statement -> Check a -> Check a
withStatement (Let name Nothing value) f = do
  typ <- checkExpr value
  withBinding name typ f

withStatement (Let name (Just typ) value) f =
  expectType value typ >> withBinding name typ f

withStatement (Expression expr) f = checkExpr expr >> f

withStatement _ f = f

checkDefinition :: Definition -> Check ()
checkDefinition (GlobalDef _ typ (Just value)) = expectType value typ
checkDefinition (FunctionDef _ _ params ret (Just body)) =
  flip (foldr withParameter) params $
  local (envResultType ?~ ret) $
  flip (foldr withStatement) body $ pure ()

checkDefinition _ = pure ()

withParameter :: Parameter -> Check a -> Check a
withParameter (Parameter name typ) = withBinding name typ

withBinding :: Sourced Identifier -> Type -> Check a -> Check a
withBinding (Sourced newSrc name) typ f = do
  mbExisting <- view $ envBindings . at name
  case mbExisting of
    Just (Sourced oldSrc _) -> shadowError name oldSrc newSrc
    Nothing -> local (envBindings . at name ?~ Sourced newSrc typ) f

-- withDefinition brings definitions in scope
withDefinition :: Definition -> Check a -> Check a
withDefinition (TopComments _) f = f

withDefinition (TypeDef (Sourced newSrc name) mbType) f = do
  mbExisting <- view $ envNamedTypes . at name
  case (mbExisting, mbType) of
    (Just (Sourced oldSrc (Just existing)), Just new) 
      | existing /= new -> shadowError name oldSrc newSrc
    (Nothing, _) -> local (envNamedTypes . at name ?~ Sourced newSrc mbType) f
    _ -> f

withDefinition (GlobalDef name typ _) f = withBinding name typ f
withDefinition (FunctionDef _ name params ret _) f = withBinding name typ f
  where typ = FunctionOf (paramType <$> params) ret

casters :: [(Sourced Identifier, Type)]
casters = bimap (Sourced Builtin . Identifier) CastTo <$>
  [ ("byte", Byte)
  , ("int", Int)
  , ("usize", Size)
  ]

check :: [Definition] -> Either [Sourced Error] ()
check defs = _Left %~ pure $
  flip runReaderT emptyEnv $
  flip (foldr (uncurry withBinding)) casters $
  flip (foldr withDefinition) defs do
    traverse_ checkDefinition defs
