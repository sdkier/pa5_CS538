{-# LANGUAGE OverloadedStrings #-}

module Parser (
  parseExpr,
  parseModule
) where

import Text.Parsec
    ( optional, (<|>), many, many1, parse, try, sepBy, ParseError )
import Text.Parsec.Text.Lazy (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Data.Text.Lazy as L

import Lexer
import Syntax

-- TODO-0: Bug! Check the github issues/pull-requests to try to fix this: 
--       https://github.com/sdiehl/write-you-a-haskell
integer :: Parser Integer
integer = Tok.integer lexer

variable :: Parser Expr
variable = Var <$> identifier

number :: (Lit -> a) -> Parser a
number c = c . LInt . fromIntegral <$> integer

bool :: (Lit -> a) -> Parser a
bool c = (reserved "True" >> return (c (LBool True)))
    <|> (reserved "False" >> return (c (LBool False)))

-- https://hackage.haskell.org/package/parsec-3.1.17.0/docs/Text-Parsec.html#g:2

-- TODO-1: Handle parsing a list
list :: (Lit -> a) -> Parser a
list c = Tok.brackets lexer (do
  elements <- expr `sepBy` reservedOp ","
  return $ c (LArray elements))

fix :: Parser Expr
fix = do
  reservedOp "fix"
  Fix <$> expr

pattern :: Parser Pattern
pattern = try pcons
      <|> try plit 
      <|> pvar
  where
    pvar = PVar <$> identifier
    plit = PLit <$> (number id <|> bool id)
    pcons = parens $ do
      p1 <- pattern
      reservedOp ":"
      p2 <- pattern
      return $ PCons p1 p2

-- TODO-2: use patterns instead of identifiers for args ^^ & VV
lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many pattern
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  Let x e1 <$> expr

letrecin :: Parser Expr
letrecin = do
  reserved "let"
  reserved "rec"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  Let x e1 <$> expr

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  If cond tr <$> expr

--- TODO-1: Add parsing for list
aexp :: Parser Expr
aexp = parens expr
  <|> list Lit      -- Add list parsing
  <|> bool Lit
  <|> number Lit
  <|> ifthen
  <|> fix
  <|> try letrecin
  <|> letin
  <|> lambda
  <|> variable

term :: Parser Expr
term = aexp >>= \x ->
                (many1 aexp >>= \xs -> return (foldl App x xs))
                <|> return x

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)


-- TODO-1: Add cons operator. Make sure you have proper associativity!
-- TODO-2: Add concat operator. Make sure you have proper associativity!
table :: Operators Expr
table = [
    [infixOp ":" (Op Cons) Ex.AssocRight],   -- Cons is right associative
    [infixOp "++" (Op Concat) Ex.AssocRight], -- Concat is right associative
    [infixOp "*" (Op Mul) Ex.AssocLeft],
    [infixOp "+" (Op Add) Ex.AssocLeft,
     infixOp "-" (Op Sub) Ex.AssocLeft],
    [infixOp "==" (Op Eql) Ex.AssocLeft]
  ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table term

type Binding = (String, Expr)

-- TODO-2: use patterns instead of identifiers for args
letdecl :: Parser Binding
letdecl = do
  reserved "let"
  name <- identifier
  args <- many pattern
  reservedOp "="
  body <- expr
  return (name, foldr Lam body args)

-- TODO-2: use patterns instead of identifiers for args
letrecdecl :: Parser (String, Expr)
letrecdecl = do
  reserved "let"
  reserved "rec"
  name <- identifier
  args <- many pattern
  reservedOp "="
  body <- expr
  return (name, Fix $ foldr Lam body (PVar name:args))

val :: Parser Binding
val = do
  ex <- expr
  return ("it", ex)

decl :: Parser Binding
decl = try letrecdecl <|> letdecl <|> val

top :: Parser Binding
top = do
  x <- decl
  optional semi
  return x

modl ::  Parser [Binding]
modl = many top

parseExpr :: L.Text -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseModule ::  FilePath -> L.Text -> Either ParseError [(String, Expr)]
parseModule = parse (contents modl)
