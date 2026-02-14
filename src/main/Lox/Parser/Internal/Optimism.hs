module Lox.Parser.Internal.Optimism(ast, declaration, statement) where

import Lox.Scanner.Token(Token(Class, Comma, Else, EOF, Equal, For, Fun, If, LeftBrace, LeftParen, Less, Print, Return, RightBrace, RightParen, Semicolon, Var, While))

import Lox.Parser.Internal.AST(
    AST(AST)
  , Expr(LiteralExpr)
  , Function(Function)
  , Literal(BooleanLit, NilLit)
  , Statement(Block, contents, DeclareVar, ExpressionStatement, FunctionStatement, IfElse, PrintStatement, ReturnStatement, WhileStatement)
  , Variable
  )

import Lox.Parser.Internal.ExpressionParser(expression)
import Lox.Parser.Internal.Parse(locOf, one, Parser, throwaway, variable, whineAbout)
import Lox.Parser.Internal.ParserError(ParserErrorType(TooMuchArguing))

import qualified Lox.Parser.Internal.AST as AST


ast :: Parser AST
ast = AST <$> (many declaration) <* (throwaway EOF)

declaration :: Parser Statement
declaration = classDeclaration <|> fnDeclaration <|> varDeclaration <|> statement

classDeclaration :: Parser Statement
classDeclaration =
  AST.Class <$>
    ((throwaway Class) *> variable) <*> (optional $ (throwaway Less) *> variable) <*>
    ((throwaway LeftBrace) *> (many methodDeclaration) <* (throwaway RightBrace))
  where
    methodDeclaration = function

fnDeclaration :: Parser Statement
fnDeclaration = FunctionStatement <$> ((throwaway Fun) *> function)

function :: Parser Function
function = Function <$> fnName <*> ((throwaway LeftParen) *> fnParams <* (throwaway RightParen)) <*> fnBlock
  where
    fnName  = variable
    fnBlock = contents <$> block

fnParams :: Parser [Variable]
fnParams = (limited existent) <|> nullary
  where
    existent  = (:) <$> variable <*> (many $ (throwaway Comma) *> variable)
    nullary   = pure []
    limited p = p >>= (\args -> if (length args) < 255 then return args else whineAbout TooMuchArguing)

varDeclaration :: Parser Statement
varDeclaration = declare <$> ((throwaway Var) *> variable) <*>
                             ((optional $ (throwaway Equal) *> expression) <* (throwaway Semicolon))
  where
    declare var initialM = DeclareVar var $ maybe (LiteralExpr NilLit var.varToken) id initialM

statement :: Parser Statement
statement = forStatement <|> ifStatement <|> printStatement <|> returnStatement <|> whileStatement <|> exprStatement <|> block

block :: Parser Statement
block = Block <$> ((throwaway LeftBrace) *> (many declaration) <* (throwaway RightBrace))

exprStatement :: Parser Statement
exprStatement = ExpressionStatement <$> expression <* (throwaway Semicolon)

printStatement :: Parser Statement
printStatement = PrintStatement <$> (locOf Print) <*> (expression <* (throwaway Semicolon))

returnStatement :: Parser Statement
returnStatement = ReturnStatement <$> (locOf Return) <*> ((optional expression) <* (throwaway Semicolon))

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
