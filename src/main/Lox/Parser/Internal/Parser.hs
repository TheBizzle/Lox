module Lox.Parser.Internal.Parser(parse) where

import Lox.Scanner.Token(Token(EOF, Equal, Print, Semicolon, Var), TokenPlus)

import Lox.Parser.Internal.ExpressionParser(expression, run)
import Lox.Parser.Internal.Parse(one, Parser, throwaway, variable, whineAbout)
import Lox.Parser.Internal.ParserError(ParserError, ParserErrorType(InvalidStatement, MissingSemicolon))

import Lox.Parser.Internal.Program(
    Expr(LiteralExpr, Variable),
    Literal(NilLit),
    Program(Program),
    Statement(DeclareVar, ExpressionStatement, PrintStatement)
  )

import qualified Data.List.NonEmpty as NE


parse :: [TokenPlus] -> Validation (NonEmpty ParserError) Program
parse = parser.run &> (either handleError handleSuccess)
  where
    handleError   = NE.singleton &> Failure
    handleSuccess = fst &> Success

parser :: Parser Program
parser = program

program :: Parser Program
program = goodProgram <|> badProgram
  where
    goodProgram = Program <$> (many declaration) <* (throwaway EOF)

    badProgram = (many declaration) *> badDeclaration

badDeclaration :: Parser a
badDeclaration = badVarDecl <|> badStatement
  where
    badVarDecl   = (throwaway Var) *> variable *> (optional $ (throwaway Equal) *> expression) *> (whineAbout MissingSemicolon)
    badStatement = expression *> whineAbout MissingSemicolon

declaration :: Parser Statement
declaration = varDeclaration <|> statement

varDeclaration :: Parser Statement
varDeclaration = declare <$> ((throwaway Var) *> variable) <*>
                             ((optional $ (throwaway Equal) *> expression) <* (throwaway Semicolon))
  where
    declare (Variable ident) initialM = DeclareVar ident $ maybe (LiteralExpr NilLit) id initialM
    declare                _        _ = error "Impossibility: Our variable isn't a variable!"

statement :: Parser Statement
statement = printStatement <|> exprStatement

exprStatement :: Parser Statement
exprStatement = ExpressionStatement <$> (expression <* (throwaway Semicolon))

printStatement :: Parser Statement
printStatement = PrintStatement <$> (one Print) <*> (expression <* (throwaway Semicolon))
