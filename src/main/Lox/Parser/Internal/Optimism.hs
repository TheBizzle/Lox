module Lox.Parser.Internal.Optimism(declaration, program, statement) where

import Lox.Scanner.Token(Token(Comma, Else, EOF, Equal, For, Fun, If, LeftBrace, LeftParen, Print, Return, RightBrace, RightParen, Semicolon, Var, While))

import Lox.Parser.Internal.ExpressionParser(expression)
import Lox.Parser.Internal.Parse(one, Parser, throwaway, variable, whineAbout)
import Lox.Parser.Internal.ParserError(ParserErrorType(TooMuchArguing))
import Lox.Parser.Internal.Program(
    Expr(LiteralExpr, Variable),
    Literal(BooleanLit, NilLit),
    Program(Program),
    Statement(Block, contents, DeclareVar, ExpressionStatement, Function, IfElse, PrintStatement, ReturnStatement, WhileStatement)
  )


program :: Parser Program
program = Program <$> (many declaration) <* (throwaway EOF)

declaration :: Parser Statement
declaration = fnDeclaration <|> varDeclaration <|> statement

fnDeclaration :: Parser Statement
fnDeclaration =  (uncurry3 Function) <$> ((throwaway Fun) *> function)

function :: Parser (Expr, [Expr], [Statement])
function = (,,) <$> fnName <*> ((throwaway LeftParen) *> fnParams <* (throwaway RightParen)) <*> fnBlock
  where
    fnName  = fullVariable
    fnBlock = contents <$> block

fnParams :: Parser [Expr]
fnParams = (limited existent) <|> nullary
  where
    existent  = (:) <$> fullVariable <*> (many $ (throwaway Comma) *> fullVariable)
    nullary   = pure []
    limited p = p >>= (\args -> if (length args) < 255 then return args else whineAbout TooMuchArguing)

varDeclaration :: Parser Statement
varDeclaration = declare <$> ((throwaway Var) *> variable) <*>
                             ((optional $ (throwaway Equal) *> expression) <* (throwaway Semicolon))
  where
    declare (vn, ident) initialM = DeclareVar vn ident $ maybe (LiteralExpr NilLit ident) id initialM

statement :: Parser Statement
statement = forStatement <|> ifStatement <|> printStatement <|> returnStatement <|> whileStatement <|> exprStatement <|> block

block :: Parser Statement
block = Block <$> ((throwaway LeftBrace) *> (many declaration) <* (throwaway RightBrace))

exprStatement :: Parser Statement
exprStatement = ExpressionStatement <$> expression <* (throwaway Semicolon)

printStatement :: Parser Statement
printStatement = PrintStatement <$> (one Print) <*> (expression <* (throwaway Semicolon))

returnStatement :: Parser Statement
returnStatement = ReturnStatement <$> (one Return) <*> ((optional expression) <* (throwaway Semicolon))

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

fullVariable :: Parser Expr
fullVariable = (uncurry Variable) <$> variable
