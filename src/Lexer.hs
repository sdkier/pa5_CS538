module Lexer where

import Text.Parsec ( alphaNum, letter, oneOf, eof, (<|>) )
import Text.Parsec.Text.Lazy ( Parser )
import qualified Data.Text.Lazy as L
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

import Data.Functor.Identity ( Identity )

type Op a = Ex.Operator L.Text () Identity a
type Operators a = Ex.OperatorTable L.Text () Identity a

reservedNames :: [String]
reservedNames = [
    "let",
    "in",
    "fix",
    "rec",
    "if",
    "then",
    "else"
  ]

reservedOps :: [String]
reservedOps = [
    "->",
    "\\",
    "++",
    "+",
    "*",
    "-",
    ":",
    "="
    -- TODO-1: Add handling for cons (:) operator
    -- TODO-2: Add handling for concat (++) operator
  ]

lexer :: Tok.GenTokenParser L.Text () Identity
lexer = Tok.makeTokenParser $ Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~," -- TODO-1: Add comma for lists
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~," -- TODO-1: Add comma for lists
  , Tok.reservedNames   = reservedNames
  , Tok.reservedOpNames = reservedOps
  , Tok.caseSensitive   = True
  }

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

semi :: Parser String
semi = Tok.semi lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r
