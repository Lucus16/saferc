module SaferC.Types where

import Control.Comonad.Cofree (Cofree(..))
import Data.Functor.Classes (Show1(..))
import Data.Functor.Classes.Generic (liftShowsPrecDefault)
import Data.Text (Text)
import GHC.Generics (Generic1)
import Numeric.Natural (Natural)
import Text.Megaparsec (SourcePos(..), unPos)

newtype Identifier = Identifier Text
  deriving (Show)

data Count
  = KnownCount Natural
  | VarCount Identifier
  | ZeroTerminated
  deriving (Show)

data MemoryState
  = Uninitialized -- Owner may not read before initializing
  | Mutable  -- Owner may mutate
  | ReadOnly -- Owner promises not to mutate but other references may mutate
  | Constant -- Guaranteed not to mutate while this reference exists
  deriving (Show)

data Type
  = Bool
  | Byte
  | Int
  | Size
  | LiteralT Literal
  | NamedType Identifier
  | OwnedPointerTo MemoryState Type
  -- | ReferenceTo MemoryState Type
  | NullableOwnedPointerTo MemoryState Type
  | Fallible Type
  | ArrayOf Count Type
  | Inert -- one value
  | NoReturn -- no values
  deriving (Show)

pattern Zero :: Type
pattern Zero = LiteralT (Integer 0)

data Purity
  = Pure
  | Impure
  deriving (Show)

data Literal
  = Integer Integer
  | Text Text
  deriving (Show)

data Source = Source
  { sourceStart :: SourcePos
  , sourceEnd :: SourcePos
  }

instance Show Source where
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

type ExprLoc = Cofree ExprF Source

type Block = [Statement]

data Parameter = Parameter Identifier Type
  deriving (Show)

data Definition
  = TypeDef Identifier (Maybe Type)
  | FunctionDef Purity Identifier [Parameter] Type (Maybe Block)
  | GlobalDef Identifier Type (Maybe ExprLoc)
  | TopComments [Text]
  deriving (Show)

data Statement
  = Let Identifier Type ExprLoc
  | Var Identifier Type (Maybe ExprLoc)
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

isIntegral :: Type -> Bool
isIntegral Bool = True
isIntegral Byte = True
isIntegral Int = True
isIntegral Size = True
isIntegral (LiteralT (Integer _)) = True
isIntegral NoReturn = True
isIntegral _ = False

(<:) :: Type -> Type -> Bool
_ <: NoReturn = True
NoReturn <: _ = False
NullableOwnedPointerTo _ _ <: Zero = True
NullableOwnedPointerTo m a <: b = OwnedPointerTo m a <: b
Fallible a <: Fallible b = a <: b
Bool <: Bool = True
Bool <: Byte = True
Bool <: Size = True
Bool <: Int = True
Bool <: NullableOwnedPointerTo _ _ = True
Bool <: LiteralT (Integer n) = 0 <= n && n < 2
Byte <: Byte = True
Byte <: Bool = True
Byte <: LiteralT (Integer n) = 0 <= n && n < 0x100
Int <: Int = True
Int <: Bool = True
Int <: LiteralT (Integer n) = -0x8000_0000 <= n && n < 0x8000_0000
Size <: Size = True
Size <: Bool = True
Size <: LiteralT (Integer n) = 0 <= n && n < 0x1_0000_0000_0000_0000
OwnedPointerTo Uninitialized tx <: OwnedPointerTo Uninitialized ty = tx <: ty
OwnedPointerTo Uninitialized tx <: OwnedPointerTo Mutable ty = tx <: ty
OwnedPointerTo Uninitialized _ <: OwnedPointerTo ReadOnly _ = False
OwnedPointerTo Mutable _ <: OwnedPointerTo Uninitialized _ = False
OwnedPointerTo Mutable tx <: OwnedPointerTo Mutable ty = tx <: ty
OwnedPointerTo Mutable _ <: OwnedPointerTo ReadOnly _ = False
OwnedPointerTo ReadOnly _ <: OwnedPointerTo Uninitialized _ = False
OwnedPointerTo ReadOnly tx <: OwnedPointerTo ReadOnly ty = tx <: ty
OwnedPointerTo ReadOnly tx <: OwnedPointerTo Mutable ty = tx <: ty
_ <: _ = False
