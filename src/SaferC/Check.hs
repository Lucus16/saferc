{-# language TemplateHaskell #-}

module SaferC.Check where

import Data.Functor (void)
import Data.Text qualified as Text
import Data.Foldable (traverse_)
import Control.Lens
import Data.Map (Map)
import Data.Map qualified as Map
import Control.Monad (unless)
import Control.Comonad (extract)
import Control.Comonad.Cofree (unwrap)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT, local, runReaderT)
import Data.PartialOrd (PartialOrd((<=)))
import Prelude hiding ((<), (>), (<=), (>=))

import SaferC.Error
import SaferC.Types

data Info = Info
  { _infoType :: Type
  , _infoMemoryState :: MemoryState
  , _infoSource :: Source
  }

makeLenses ''Info

data Env = Env
  { _envResultType :: Maybe Type
  , _envNamedTypes :: Map Identifier (Sourced (Maybe Type))
  , _envBindings :: Map Identifier Info
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
  unless (found <= expected) $ typeError expr (show expected) found

expectIndex :: ExprLoc -> Check ()
expectIndex x = checkExpr x >>= \case
  IntegerLiteral i | 0 <= i -> pure ()
  Integral Byte -> pure ()
  Integral Size -> pure ()
  t -> typeError x "unsigned integer" t

expectNullable :: ExprLoc -> Check Type
expectNullable x = do
  tx <- checkExpr x
  case tx of
    Nullable tx'  -> pure tx'
    --Integral { }  -> pure tx
    --Bool { }      -> pure tx
    --PointerTo { } -> pure tx
    t -> typeError x "nullable" t

expectArray :: ExprLoc -> Check Type
expectArray a = checkExpr a >>= go
  where
    go :: Type -> Check Type
    go (ArrayOf _ _ tx) = pure tx
    go (PointerTo _ t) = go t
    go ta = typeError a "array" ta

expectComparable :: ExprLoc -> ExprLoc -> Check Type
expectComparable x y = do
    tx <- checkExpr x
    expectType y tx
    pure Bool

checkCast :: Source -> Type -> Type -> Check Type
--checkCast _ (Integral toStorageType toRange) (Integral _ fromRange)
--  | fromRange <= toRange = pure $ Integral toStorageType fromRange
--  | otherwise = pure $ Fallible $ Integral toStorageType (intersectRange fromRange toRange)
checkCast _ (Integral toStorageType) (Integral fromStoregeType)
  | maxRange fromStoregeType <= maxRange toStorageType = pure $ Integral toStorageType
  | otherwise = pure $ Fallible $ Integral toStorageType
checkCast src castTo castFrom
  | castTo == castFrom = pure castTo
  | otherwise = throwError $ Sourced src $ TypeError expected castFrom
  where expected = "type compatible with " <> show castTo

checkLValue :: ExprLoc -> Check Type
checkLValue _ = pure $ Integral Int

typeUnion :: Type -> Sourced Type -> Check Type
typeUnion tx (Sourced tySrc ty)
  | ty <= tx  = pure tx
  | tx <= ty  = pure ty
  | otherwise = throwError $ Sourced tySrc $ TypeError (show tx) ty

checkExpr :: ExprLoc -> Check Type
checkExpr expr = case unwrap expr of
  Literal (Integer 0) -> pure $ Nullable NoReturn
  Literal (Integer i) -> pure $ IntegerLiteral i
  Literal (Text t) -> pure $ PointerTo Constant $
    ArrayOf ZeroTerminated (KnownCount (fromIntegral (Text.length t))) (Integral Byte)
  Equal x y -> expectComparable x y
  Unequal x y -> expectComparable x y
  LessThan x y -> expectComparable x y
  LessOrEqual x y -> expectComparable x y
  And x y -> do
    void $ expectNullable x
    expectNullable y
  Or x y -> do
    tx <- checkExpr x >>= \case
      Nullable tx -> pure tx
      tx -> typeError x "nullable" tx
    checkExpr y >>= \case
      Nullable ty -> Nullable <$> typeUnion tx (Sourced (extract y) ty)
      ty -> typeUnion tx (Sourced (extract y) ty)
  OrElse x y -> do
    tx <- checkExpr x
    ty <- checkExpr y
    case tx of
      Fallible tx'
        | ty <= tx' -> pure tx'
        | otherwise -> typeError y (show tx') ty
      _ -> typeError x "fallible operation" tx
  Not x -> expectNullable x >> pure Bool
  AddressOf x -> do
    tx <- checkLValue x
    pure (PointerTo undefined tx)
  Deref x -> do
    tx <- checkExpr x
    case tx of
      PointerTo mx tx' | mx /= Uninitialized -> pure tx'
      _ -> typeError x "non-null non-void pointer" tx
  Index a i -> do
    expectIndex i
    Fallible <$> expectArray a
  Variable name -> view (envBindings . at name) >>= \case
    Just info -> pure $ info ^. infoType
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

withStatement (Assignment lvalue value) f = case unwrap lvalue of
  Variable name ->
    view (envBindings . at name) >>= \case
      Nothing -> throwError $ Sourced (extract lvalue) $ NotDefined name
      Just info -> expectType value (info ^. infoType) >> f
  _ -> f

withStatement (Expression expr) f = checkExpr expr >> f

withStatement (If cond block1 block2) f =
  expectNullable cond >> checkBlock block1 >> checkBlock block2 >> f

withStatement (While cond body) f =
  expectNullable cond >> checkBlock body >> f

withStatement _ f = f

checkBlock :: Block -> Check ()
checkBlock defs = flip (foldr withStatement) defs $ pure ()

checkDefinition :: Definition -> Check ()
checkDefinition (GlobalDef _ typ (Just value)) = expectType value typ
checkDefinition (FunctionDef _ _ params ret (Just body)) =
  flip (foldr withParameter) params $
  local (envResultType ?~ ret) $
  checkBlock body

checkDefinition _ = pure ()

withParameter :: Parameter -> Check a -> Check a
withParameter (Parameter name typ) = withBinding name typ

withBinding :: Sourced Identifier -> Type -> Check a -> Check a
withBinding (Sourced newSrc name) typ f = do
  mbExisting <- view $ envBindings . at name
  case mbExisting of
    Just oldInfo -> shadowError name (oldInfo ^. infoSource) newSrc
    Nothing -> local (envBindings . at name ?~ Info typ Constant newSrc) f

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
casters = bimap (Sourced Builtin . Identifier) (CastTo . Integral) <$>
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
