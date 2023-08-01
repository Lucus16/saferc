module SaferC where

import Control.Applicative ((<|>))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree(..))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Function ((&))
import Data.Functor (void)
import Data.Functor.Classes (Show1(..))
import Data.Functor.Classes.Generic (liftShowsPrecDefault)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import GHC.Generics (Generic1)
import Numeric.Natural (Natural)
import Text.Megaparsec
  (MonadParsec, Parsec, SourcePos(..), Token, Tokens, TraversableStream,
  anySingle, between, chunk, eof, getSourcePos, many, notFollowedBy, optional,
  satisfy, sepBy, single, some, takeWhile1P, takeWhileP, unPos)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer qualified as L
  (decimal, hexadecimal, lexeme, signed, symbol)

type Parser = Parsec Void Text

newtype Identifier = Identifier Text
  deriving (Show)

data Count
  = KnownCount Natural
  | VarCount Identifier
  | ZeroTerminated
  deriving (Show)

data MemoryState
  = Uninitialized -- Including partially uninitialized
  | Mutable
  | ReadOnly
  deriving (Show)

data Type
  = Int
  | USize
  | Byte
  | NamedType Identifier
  | PointerTo MemoryState Type
  | ReferenceTo MemoryState Type
  | Nullable Type
  | ArrayOf Count Type
  | Inert -- one value
  | NoReturn -- no values
  deriving (Show)

data Purity
  = Pure
  | Impure
  deriving (Show)

data Literal
  = Integer Integer
  | Text Text
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

purity :: Parser Purity
purity = Pure <$ keyword "pure" <|> pure Impure

functionDef :: Parser Definition
functionDef =
  FunctionDef <$> purity <* keyword "fn"
    <*> identifier
    <*> parameters
    <*> (fromMaybe Inert <$> optional (symbol ":" *> type_))
    <*> ((Just <$> block) <|> (Nothing <$ symbol ";"))

parameter :: Parser Parameter
parameter = Parameter <$> identifier <* symbol ":" <*> type_

parameters :: Parser [Parameter]
parameters = between (symbol "(") (symbol ")") (parameter `sepBy` symbol ",")

block :: Parser Block
block = between (symbol "{") (symbol "}") (many statement)

letStatement :: Parser Statement
letStatement = Let <$ keyword "let"
  <*> identifier
  <*> (fromMaybe Int <$> optional (symbol ":" >> type_))
  <*> (symbol "=" >> expression)
  <* symbol ";"

varStatement :: Parser Statement
varStatement = Var <$ keyword "var"
  <*> identifier
  <*> (fromMaybe Int <$> optional (symbol ":" >> type_))
  <*> optional (symbol "=" >> expression)
  <* symbol ";"

-- Only statements that transfer control may appear in an if without braces.
ifStatement :: Parser Statement
ifStatement = If <$ keyword "if"
  <*> expression
  <*> (pure <$> returnStatement <|> block)
  <*> (fromMaybe [] <$> optional (keyword "else" *> ((pure <$> ifStatement) <|> block)))

whileStatement :: Parser Statement
whileStatement = While <$ keyword "while" <*> expression <*> block

returnStatement :: Parser Statement
returnStatement = Expression <$> located (Return <$ keyword "return" <*> optional expression <* symbol ";")

statement :: Parser Statement
statement =
  ifStatement
  <|> letStatement
  <|> returnStatement
  <|> varStatement
  <|> whileStatement
  <|> Comments <$> some comment
  <|> (&) <$> expression <*> (flip Assignment <$ symbol "=" <*> expression <|> pure Expression) <* symbol ";"

expression :: Parser ExprLoc
expression = or_

or_ :: Parser ExprLoc
or_ = manyInfixl and_ $
  OrElse <$ keyword "or else"
  <|> Or <$ keyword "or"

and_ :: Parser ExprLoc
and_ = manyInfixl return_ $ And <$ keyword "and"

return_ :: Parser ExprLoc
return_ = located (Return <$ keyword "return" <*> optional notExpr) <|> notExpr

notExpr :: Parser ExprLoc
notExpr = manyPrefix comparison $ Not <$ keyword "not"

comparison :: Parser ExprLoc
comparison = manyInfixl prefixExpr $
  Equal <$ symbol "=="
  <|> Unequal <$ symbol "!="
  <|> LessThan <$ symbol "<"
  <|> LessOrEqual <$ symbol "<="

postfixExpr :: Parser ExprLoc
postfixExpr = manyPostfix term $
  flip Call <$ symbol "(" <*> (expression `sepBy` symbol ",") <* symbol ")"
  <|> flip Index <$ symbol "[" <*> expression <* symbol "]"

prefixExpr :: Parser ExprLoc
prefixExpr = manyPrefix postfixExpr $ AddressOf <$ symbol "&"

signed :: Num a => Parser a -> Parser a
signed = L.signed (pure ())

literal :: Parser Literal
literal =
  Integer <$ (chunk "0x" <|> chunk "0X") <*> lexeme (signed L.hexadecimal)
  <|> Integer <$> lexeme (signed L.decimal)
  <|> Text <$ single '"' <*> (Text.concat <$> many (Text.singleton <$> char)) <* single '"'
  where
    char :: Parser Char
    char = (single '\\' *> anySingle) <|> satisfy (not . (`Text.elem` "\"\\"))

term :: Parser ExprLoc
term = located $
  Variable <$> identifier
  <|> Literal <$> literal

typeDef :: Parser Definition
typeDef = keyword "type" >> TypeDef <$> identifier <*> optional type_ <* symbol ";"

definition :: Parser Definition
definition = TopComments <$> some comment
  <|> typeDef
  <|> functionDef
  <|> globalDef

globalDef :: Parser Definition
globalDef = GlobalDef <$ keyword "var" <*> identifier <* symbol ":" <*> type_ <*> optional expression <* symbol ";"

arraySize :: Parser Count
arraySize =
  KnownCount <$> lexeme L.decimal
  <|> VarCount <$> identifier
  <|> ZeroTerminated <$ symbol ":" <* symbol "0"

memoryState :: Parser MemoryState
memoryState = Uninitialized <$ keyword "uninit"
  <|> Mutable <$ keyword "mut"
  <|> pure ReadOnly

type_ :: Parser Type
type_ = Nullable <$ symbol "?" <*> type_
  <|> PointerTo <$ symbol "*" <*> memoryState <*> type_
  <|> ReferenceTo <$ symbol "&" <*> memoryState <*> type_
  <|> ArrayOf <$ symbol "[" <*> arraySize <* symbol "]" <*> type_
  <|> NamedType <$> identifier

comment :: Parser Text
comment = lexeme $ chunk "#" *> manyP (\c -> c /= '\r' && c /= '\n') <* newline

manyPrefix
  :: (MonadParsec e s m, TraversableStream s, Functor f)
  => m (Cofree f Source)
  -> m (Cofree f Source -> f (Cofree f Source))
  -> m (Cofree f Source)
manyPrefix pArg pCon = flip (foldl (&)) <$> many prefix <*> pArg
  where
    prefix = do
      start <- getSourcePos
      con <- pCon
      pure \arg -> Source start (sourceEnd (extract arg)) :< con arg

manyPostfix
  :: (MonadParsec e s m, TraversableStream s, Functor f)
  => m (Cofree f Source)
  -> m (Cofree f Source -> f (Cofree f Source))
  -> m (Cofree f Source)
manyPostfix pArg pCon = foldl (&) <$> pArg <*> many do
  con <- pCon
  end <- getSourcePos
  pure \arg -> Source (sourceStart (extract arg)) end :< con arg

manyInfixl
  :: (MonadParsec e s m, TraversableStream s, Functor f)
  => m (Cofree f Source)
  -> m (Cofree f Source -> Cofree f Source -> f (Cofree f Source))
  -> m (Cofree f Source)
manyInfixl pArg pCon = foldl (&) <$> pArg <*> many do
  con <- pCon
  rhs <- pArg
  pure \lhs -> extract lhs <> extract rhs :< con lhs rhs

located
  :: (MonadParsec e s m, TraversableStream s, Functor f)
  => m (f (Cofree f Source)) -> m (Cofree f Source)
located pX = do
  start <- getSourcePos
  x <- pX
  end <- getSourcePos
  pure $ Source start end :< x

manyP :: MonadParsec e s m => (Token s -> Bool) -> m (Tokens s)
manyP = takeWhileP Nothing

someP :: MonadParsec e s m => (Token s -> Bool) -> m (Tokens s)
someP = takeWhile1P Nothing

space :: Parser ()
space = void $ manyP isSpace

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol ::Text -> Parser ()
symbol = void . L.symbol space

identChar :: Char -> Bool
identChar c = isAlpha c || isDigit c || c == '_'

identFirstChar :: Char -> Bool
identFirstChar c = isAlpha c || c == '_'

identifier :: Parser Identifier
identifier = fmap Identifier $ lexeme $
  Text.cons <$> satisfy identFirstChar <*> manyP identChar

keyword :: Text -> Parser Text
keyword name = chunk name <* notFollowedBy (satisfy identChar) <* space

file :: Parser [Definition]
file = many definition <* eof
