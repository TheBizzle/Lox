module Lox.Parser.Internal.Optimism(declaration, program, statement) where

import Lox.Scanner.Token(Token(Else, EOF, Equal, For, If, LeftBrace, LeftParen, Print, RightBrace, RightParen, Semicolon, Var, While))

import Lox.Parser.Internal.ExpressionParser(expression)
import Lox.Parser.Internal.Parse(one, Parser, throwaway, variable)
import Lox.Parser.Internal.Program(
    Expr(LiteralExpr),
    Literal(BooleanLit, NilLit),
    Program(Program),
    Statement(Block, DeclareVar, ExpressionStatement, IfElse, PrintStatement, WhileStatement)
  )


program :: Parser Program
program = Program <$> (many declaration) <* (throwaway EOF)

declaration :: Parser Statement
declaration = varDeclaration <|> statement

varDeclaration :: Parser Statement
varDeclaration = declare <$> ((throwaway Var) *> variable) <*>
                             ((optional $ (throwaway Equal) *> expression) <* (throwaway Semicolon))
  where
    declare (vn, ident) initialM = DeclareVar vn ident $ maybe (LiteralExpr NilLit ident) id initialM

statement :: Parser Statement
statement = forStatement <|> ifStatement <|> printStatement <|> whileStatement <|> exprStatement <|> block

block :: Parser Statement
block = Block <$> ((throwaway LeftBrace) *> (many declaration) <* (throwaway RightBrace))

exprStatement :: Parser Statement
exprStatement = ExpressionStatement <$> expression <* (throwaway Semicolon)

printStatement :: Parser Statement
printStatement = PrintStatement <$> (one Print) <*> (expression <* (throwaway Semicolon))

forStatement :: Parser Statement
forStatement = buildLoop <$>
                 (one For) <*>
                 ((throwaway LeftParen) *> initializer) <*>
                 ((optional expression) <* (throwaway  Semicolon)) <*>
                 ((optional expression) <* (throwaway RightParen)) <*>
                 statement
  where
    initializer = varDeclaration <|> exprStatement <|> ((const $ Block []) <$> (one Semicolon))

    buildLoop forT init condM incM body = Block [init, loop]
      where
        cond     = maybe (LiteralExpr (BooleanLit True) forT) id condM
        inc      = maybeToList $ map ExpressionStatement incM
        fullBody = Block $ [body] <> inc
        loop     = WhileStatement cond fullBody

ifStatement :: Parser Statement
ifStatement = ifElse <|> plainIf
  where
    ifElse = (\ant con alt -> IfElse ant con $ Just alt) <$>
               ((throwaway If) *> (throwaway LeftParen) *> expression <* (throwaway RightParen)) <*>
               statement <*>
               ((throwaway Else) *> statement)

    plainIf = (\ant con -> IfElse ant con Nothing) <$>
                ((throwaway If) *> (throwaway LeftParen) *> expression <* (throwaway RightParen)) <*>
                statement

whileStatement :: Parser Statement
whileStatement = WhileStatement <$>
                   ((throwaway While) *> (throwaway LeftParen) *> expression <* (throwaway RightParen)) <*>
                   statement
