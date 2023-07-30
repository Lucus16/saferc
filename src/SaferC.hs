module SaferC where

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Function ((&))
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Numeric.Natural (Natural)
import Text.Megaparsec
  (MonadParsec, Parsec, Token, Tokens, anySingle, between, chunk, eof, many,
  notFollowedBy, optional, satisfy, sepBy, single, some, takeWhile1P,
  takeWhileP)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer qualified as L
  (decimal, hexadecimal, lexeme, signed, symbol)

type Parser = Parsec Void Text

newtype Identifier = Identifier Text
  deriving (Show)

data Count
  = UnknownCount
  | KnownCount Natural
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

data Expression
  = Literal Literal
  | Variable Identifier
  | Access Expression Identifier
  | Deref Expression
  | Index Expression Expression
  | Call Expression [Expression]
  | Equal Expression Expression
  | Unequal Expression Expression
  | LessThan Expression Expression
  | LessOrEqual Expression Expression
  | And Expression Expression
  | Or Expression Expression
  | OrElse Expression Expression
  | Not Expression
  | AddressOf Expression
  | Return (Maybe Expression)
  deriving (Show)

type Block = [Statement]

data Parameter = Parameter Identifier Type
  deriving (Show)

data Definition
  = TypeDef Identifier (Maybe Type)
  | FunctionDef Purity Identifier [Parameter] Type (Maybe Block)
  | GlobalDef Identifier Type (Maybe Expression)
  | TopComments [Text]
  deriving (Show)

data Statement
  = Let Identifier Type Expression
  | Var Identifier Type (Maybe Expression)
  | If Expression Block Block
  | While Expression Block
  | Break
  | Continue
  | Comments [Text]
  | Expression Expression
  | Assignment Expression Expression
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
returnStatement = Expression . Return <$ keyword "return" <*> optional expression <* symbol ";"

statement :: Parser Statement
statement =
  ifStatement
  <|> letStatement
  <|> returnStatement
  <|> varStatement
  <|> whileStatement
  <|> Comments <$> some comment
  <|> (&) <$> expression <*> (flip Assignment <$ symbol "=" <*> expression <|> pure Expression) <* symbol ";"

expression :: Parser Expression
expression = or_

or_ :: Parser Expression
or_ = foldl (&) <$> and_ <*> many
  (flip <$> (OrElse <$ keyword "or else" <|> Or <$ keyword "or") <*> and_)

and_ :: Parser Expression
and_ = foldl (&) <$> return_ <*> many (flip And <$ keyword "and" <*> return_)

return_ :: Parser Expression
return_ = Return <$ keyword "return" <*> optional not_ <|> not_

not_ :: Parser Expression
not_ = flip (foldl (&)) <$> many (Not <$ keyword "not") <*> comparison

comparison :: Parser Expression
comparison = do
  lhs <- prefix
  cmps <- many $ flip <$>
    (Equal <$ symbol "=="
    <|> Unequal <$ symbol "!="
    <|> LessThan <$ symbol "<"
    <|> LessOrEqual <$ symbol "<=") <*> prefix
  pure $ foldl (&) lhs cmps

postfix :: Parser Expression
postfix = do
  t <- term
  postfixes <- many $
    flip Call <$ symbol "(" <*> (expression `sepBy` symbol ",") <* symbol ")"
    <|> flip Index <$ symbol "[" <*> expression <* symbol "]"
  pure $ foldl (&) t postfixes

prefix :: Parser Expression
prefix = flip (foldl (&))
  <$> many (AddressOf <$ symbol "&")
  <*> postfix

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

term :: Parser Expression
term =
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
  <|> pure UnknownCount

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
