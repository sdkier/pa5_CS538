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
import Debug.Trace (trace)

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
pattern = do
    trace "Starting pattern parse" $ return ()
    p1 <- simplePattern
    trace ("Got first pattern: " ++ show p1) $ return ()
    let rest = try $ do
            trace "Looking for cons" $ return ()
            reservedOp ":"
            p2 <- pattern  -- Recursively parse rest of pattern
            trace ("Building cons pattern: " ++ show p1 ++ ":" ++ show p2) $ return ()
            return $ PCons p1 p2
        nocons = trace "No cons found, returning simple pattern" $ return p1
    rest <|> nocons

simplePattern :: Parser Pattern
simplePattern = trace "In simplePattern" $ 
    try (trace "Trying empty" pempty)
    <|> try (trace "Trying var" pvar)
    <|> try (trace "Trying num" pnum)
    <|> try (trace "Trying bool" pbool)
    <|> parens pattern

pvar :: Parser Pattern
pvar = PVar <$> identifier

pnum :: Parser Pattern
pnum = PLit . LInt . fromIntegral <$> integer

pbool :: Parser Pattern
pbool = (reserved "True" >> return (PLit (LBool True)))
    <|> (reserved "False" >> return (PLit (LBool False)))

pempty :: Parser Pattern
pempty = Tok.brackets lexer (return $ PLit (LArray []))


-- TODO-2: use patterns instead of identifiers for args
lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many (try $ parens pattern <|> simplePattern)
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
aexp = trace "Parsing atomic expression" $ 
    parens expr
    <|> list Lit
    <|> bool Lit
    <|> try (trace "Trying number" $ number Lit)
    <|> try (trace "Trying variable" $ variable)
    <|> ifthen
    <|> fix
    <|> try letrecin
    <|> letin
    <|> lambda

term :: Parser Expr
term = trace "Parsing term" $ aexp >>= \x ->
                (many1 aexp >>= \xs -> trace ("Found application terms: " ++ show xs) $ return (foldl App x xs))
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
expr = trace "Parsing expression" $ Ex.buildExpressionParser table term

type Binding = (String, Expr)

-- TODO-2: use patterns instead of identifiers for args
letdecl :: Parser Binding
letdecl = trace "In letdecl" $ do
  reserved "let"
  name <- identifier
  trace ("Parsing patterns for: " ++ name) $ return ()
  args <- many pattern  -- Changed: collect all patterns
  reservedOp "="
  body <- expr
  -- Create nested lambdas for all patterns
  return (name, foldr Lam body args)


-- TODO-2: use patterns instead of identifiers for args
letrecdecl :: Parser (String, Expr)
letrecdecl = do
  reserved "let"
  reserved "rec"
  name <- identifier
  args <- many (try $ parens pattern <|> simplePattern)
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
