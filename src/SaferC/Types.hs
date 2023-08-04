module SaferC.Types where

import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Classes (Show1(..))
import Data.Functor.Classes.Generic (liftShowsPrecDefault)
import Data.Text (Text)
import GHC.Generics (Generic1)
import Numeric.Natural (Natural)
import Text.Megaparsec (SourcePos(..), pos1, unPos)
import Data.PartialOrd (PartialOrd((<=), (>=)))
import Prelude hiding ((<), (>), (<=), (>=))

newtype Identifier = Identifier Text
  deriving (Eq, Ord, Show)

data Count
  = UnknownCount
  | KnownCount Natural
  | VarCount Identifier
  deriving (Eq, Show)

data ZeroTerminated
  = ZeroTerminated
  | NotZeroTerminated
  deriving (Eq, Show)

data MemoryState
  = Uninitialized -- Owner may not read before initializing
  | Mutable  -- Owner may mutate
  | ReadOnly -- Owner promises not to mutate but other references may mutate
  | Constant -- Guaranteed not to mutate while this reference exists
  deriving (Eq, Show)

data StorageType
  = Byte
  | Int
  | Size
  deriving (Eq, Show)

data Type
  = NonZero StorageType
  | IntegerLiteral Integer
  | Null
  | NonNullBool
  | NamedType Identifier
  | Nullable Type -- ^ Not all types can be stored nullable. And x y : Nullable tx
  | Fallible Type -- ^ Only exists as intermediate type, cannot be stored.
  | PointerTo MemoryState Type
  | ArrayOf ZeroTerminated Count Type
  | FunctionOf [Type] Type
  | CastTo Type -- ^ The type of "functions" that cast their argument.
  | Void -- ^ Only used in pointers.
  | Inert -- ^ Only one value, no information.
  | NoReturn -- ^ Does not have values.
  deriving (Eq, Show)

pattern Bool :: Type
pattern Bool = Nullable NonNullBool

pattern Integral :: StorageType -> Type
pattern Integral s = Nullable (NonZero s)

maxRange :: StorageType -> Range
maxRange t = uncurry (Range True) $
  case t of
    Byte -> (0, 0xff)
    Int -> (-0x8000_0000, 0x7fff_ffff)
    Size -> (0, 0xffff_ffff_ffff_ffff)

inRange :: Integer -> Range -> Bool
inRange 0 range = rangeZero range
inRange i range = rangeMin range <= i && i <= rangeMax range

data Range = Range
  { rangeZero :: Bool
  , rangeMin :: Integer
  , rangeMax :: Integer
  } deriving (Eq, Show)

instance PartialOrd Range where
  x <= y =
    (not (rangeZero x) || rangeZero y)
    && rangeMin x >= rangeMin y
    && rangeMax x <= rangeMax y

intersectRange :: Range -> Range -> Range
intersectRange x y = Range
  { rangeZero = rangeZero x && rangeZero y
  , rangeMin = max (rangeMin x) (rangeMin y)
  , rangeMax = min (rangeMax x) (rangeMax y)
  }

data Purity
  = Pure
  | Idempotent
  | Impure
  deriving (Show)

data Literal
  = Integer Integer
  | Text Text
  deriving (Eq, Show)

data Sourced a = Sourced
  { source :: Source
  , unSourced :: a
  }

instance Show a => Show (Sourced a) where
  show (Sourced src x) = show src <> ": " <> show x

data Source
  = Source { sourceStart :: SourcePos, sourceEnd :: SourcePos }
  | Builtin

instance Show Source where
  show Builtin = "builtin"
  show src =
    foo sourceName
    <> ":" <> foo (show . unPos . sourceLine)
    <> ":" <> foo (show . unPos . sourceColumn)
    where
      foo a
        | x == y = x
        | otherwise = x <> "-" <> y
        where
          x = a (sourceStart src)
          y = a (sourceEnd src)

instance Semigroup Source where
  x <> y = Source
    { sourceStart = min (sourceStart x) (sourceStart y)
    , sourceEnd = max (sourceEnd x) (sourceEnd y)
    }

instance Monoid Source where
  mempty = Source unknown unknown
    where unknown = SourcePos "unknown" pos1 pos1

type ExprLoc = Cofree ExprF Source

type Block = [Statement]

data Parameter = Parameter
  { paramName :: Sourced Identifier
  , paramType :: Type
  } deriving (Show)

data Definition
  = TypeDef (Sourced Identifier) (Maybe Type)
  | FunctionDef Purity (Sourced Identifier) [Parameter] Type (Maybe Block)
  | GlobalDef (Sourced Identifier) Type (Maybe ExprLoc)
  | TopComments [Text]
  deriving (Show)

data Statement
  = Let (Sourced Identifier) (Maybe Type) ExprLoc
  | Var (Sourced Identifier) Type (Maybe ExprLoc)
  | If ExprLoc Block Block
  | While ExprLoc Block
  | Break
  | Continue
  | Comments [Text]
  | Expression ExprLoc
  | Assignment ExprLoc ExprLoc
  deriving (Show)

data ExprF e
  = Literal Literal
  | Variable Identifier
  | Access e Identifier
  | Deref e
  | Index e e
  | Call e [e]
  | Equal e e
  | Unequal e e
  | LessThan e e
  | LessOrEqual e e
  | And e e
  | Or e e
  | OrElse e e
  | Not e
  | AddressOf e
  | Return (Maybe e)
  deriving (Functor, Generic1, Show)

instance Show1 ExprF where
  liftShowsPrec = liftShowsPrecDefault

-- x <= y should be read as:
--    a value of type x can be assigned to a variable of type y
instance PartialOrd Type where
  NoReturn     <= _            = True
  _            <= NoReturn     = False
  NonZero x    <= NonZero y    = maxRange x <= maxRange y
  Fallible x   <= Fallible y   = x <= y
  x            <= Fallible y   = x <= y
  IntegerLiteral 0 <= Nullable _ = True
  IntegerLiteral i <= Integral y = i `inRange` maxRange y
  Nullable x   <= Nullable y   = x <= y
  x            <= Nullable y   = x <= y
  NamedType x  <= NamedType y  = x == y

  PointerTo Constant x <= PointerTo ReadOnly y = x <= y
  PointerTo _ x <= PointerTo _ (ArrayOf _ (KnownCount 1) y) = x <= y
  PointerTo _ (ArrayOf _ (KnownCount 1) x) <= PointerTo _ y = x <= y
  ArrayOf ZeroTerminated _ x <= ArrayOf ZeroTerminated UnknownCount y = x <= y
  PointerTo _ _ <= PointerTo _ Void = True
  PointerTo _ x <= PointerTo _ y = x <= y

  _ <= _ = False

--PointerTo Uninitialized tx <: PointerTo Uninitialized ty = tx <: ty
--PointerTo Uninitialized tx <: PointerTo Mutable ty = tx <: ty
--PointerTo Uninitialized _ <: PointerTo ReadOnly _ = False
--PointerTo Mutable _ <: PointerTo Uninitialized _ = False
--PointerTo Mutable tx <: PointerTo Mutable ty = tx <: ty
--PointerTo Mutable _ <: PointerTo ReadOnly _ = False
--PointerTo ReadOnly _ <: PointerTo Uninitialized _ = False
--PointerTo ReadOnly tx <: PointerTo ReadOnly ty = tx <: ty
--PointerTo ReadOnly tx <: PointerTo Mutable ty = tx <: ty
