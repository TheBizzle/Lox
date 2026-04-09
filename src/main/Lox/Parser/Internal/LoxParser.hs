module Lox.Parser.Internal.LoxParser(parse) where

import Lox.Scanner.Token(
    Token(loc)
  , TokenType(Class, Comma, Else, EOF, Equal, For, Fun, If, LeftBrace, LeftParen, Less, Print, Return, RightBrace, RightParen, Semicolon, Var, While)
  )

import Lox.Parser.Internal.AST(
    AST(AST)
  , Expr(LiteralExpr)
  , Function(Function)
  , Literal(BooleanLit, NilLit)
  , Statement(Block, DeclareVar, ExpressionStatement, FunctionStatement, IfElse, PrintStatement, ReturnStatement, WhileStatement)
  , Variable(varToken)
  )

import Lox.Parser.Internal.ExpressionParser(expression)

import Lox.Parser.Internal.Parser(
    atMost, cryAbout, dummyToken, multiError, one
  , Parsed(Backtrack, Errors, Parsed)
  , Parser, require, run, throwaway, variable
  )

import Lox.Parser.Internal.ParserError(
    ParserError(ParserError)
  , ParserErrorType(ExpectedBraceBeforeBody, ExpectedIdentifier, ExpectedParenAfterParams, ExpectedSuperName, Incomprehensible, InvalidExpression, TooMuchParaming, UnfinishedStmt)
  )

import qualified Data.List.NonEmpty      as NE
import qualified Lox.Parser.Internal.AST as AST


parse :: [Token] -> Validation (NonEmpty ParserError) AST
parse = parser.run &> (
    \case
      Backtrack       -> Failure $ NE.singleton $ ParserError Incomprehensible dummyToken
      Errors es       -> Failure es
      Parsed (ast, _) -> Success ast
  )

parser :: Parser AST
parser =
  do
    decls <- many declaration
    eofM  <- optional $ throwaway EOF
    case eofM of
      Nothing -> cryAbout InvalidExpression -- If there's unconsumed input...
      Just _  -> pure $ AST decls

declaration :: Parser Statement
declaration = classDeclaration <|> fnDeclaration <|> varDeclaration <|> statement

classDeclaration :: Parser Statement
classDeclaration =
  do
    throwaway Class
    name       <- variable
    lessM      <- optional $ throwaway Less
    superNameM <-
      case lessM of
        Nothing -> pure Nothing
        Just _  -> Just <$> (variable <|> cryAbout ExpectedSuperName)
    require LeftBrace
    methods <- many function
    require RightBrace
    pure $ AST.Class name superNameM methods

fnDeclaration :: Parser Statement
fnDeclaration =
  do
    throwaway Fun
    func <- function
    pure $ FunctionStatement func

function :: Parser Function
function =
  do
    name   <- variable
    throwaway LeftParen
    params <- fnParams   <|> pure []
    throwaway RightParen <|> cryAbout ExpectedParenAfterParams
    throwaway LeftBrace  <|> cryAbout ExpectedBraceBeforeBody
    fnBody <- many declaration
    require RightBrace
    pure $ Function name params fnBody
  where
    fnParams =
      do
        param1 <- variable
        params <- atMost TooMuchParaming 254 varToken $ (throwaway Comma) *> variable
        pure $ param1:params

varDeclaration :: Parser Statement
varDeclaration =
  do
    throwaway Var
    name     <- variable <|> cryAbout ExpectedIdentifier
    initialM <- optional $ do
      throwaway Equal
      expressionOrCry
    let initial = maybe (LiteralExpr NilLit name.varToken) id initialM
    require Semicolon
    pure $ DeclareVar name initial

statement :: Parser Statement
statement = forStatement <|> ifStatement <|> printStatement <|> returnStatement <|> whileStatement <|> exprStatement <|> block

block :: Parser Statement
block =
  do
    throwaway LeftBrace
    decls <- many declaration
    require RightBrace
    pure $ Block decls

exprStatement :: Parser Statement
exprStatement =
  do
    expr <- expression
    require Semicolon
    pure $ ExpressionStatement expr

printStatement :: Parser Statement
printStatement =
  do
    token <- one Print
    expr  <- expressionOrCry
    require Semicolon
    pure $ PrintStatement token.loc expr

returnStatement :: Parser Statement
returnStatement =
  do
    token <- one Return
    exprM <- optional expression
    require Semicolon
    pure $ ReturnStatement token exprM

forStatement :: Parser Statement
forStatement =
  do
    forToken <- one For
    require LeftParen
    initV    <- initializerOrBlockV
    condV    <- conditionOrBlockV
    require Semicolon
    incV     <- incrementOrBlockV
    rpToken  <- one RightParen
    body     <- statementOrCry
    let pairV    = (,) <$> initV <*> condV
    let defaultF = NE.singleton $ ParserError UnfinishedStmt rpToken
    validation ((<> defaultF) &> multiError) (
      \(init, condM) ->
        validation (NE.singleton &> multiError) (
          \incM ->
            pure $ buildLoop forToken init condM incM body
        ) incV
      ) pairV
  where
    buildLoop forT init condM incM body =
        Block [init, loop]
      where
        cond     = maybe (LiteralExpr (BooleanLit True) forT) id condM
        inc      = maybeToList $ map ExpressionStatement incM
        fullBody = Block $ [body] <> inc
        loop     = WhileStatement cond fullBody

    initializerOrBlockV :: Parser (Validation (NonEmpty ParserError) Statement)
    initializerOrBlockV = good <|> junk
      where
        good =
          do
            stmt <- varDeclaration <|> exprStatement <|> emptyInit
            pure $ Success stmt
        emptyInit =
          do
            throwaway Semicolon
            pure $ Block []
        junk =
          do
            token <- one LeftBrace
            throwaway RightBrace
            throwaway Semicolon
            pure $ Failure $ NE.singleton $ ParserError InvalidExpression token

    conditionOrBlockV :: Parser (Validation (NonEmpty ParserError) (Maybe Expr))
    conditionOrBlockV = expr <|> junk <|> empty
      where
        empty = pure $ Success Nothing
        expr =
          do
            good <- expression
            pure $ Success $ Just good
        junk =
          do
            token <- one LeftBrace
            throwaway RightBrace
            pure $ Failure $ NE.singleton $ ParserError InvalidExpression token

    incrementOrBlockV :: Parser (Validation ParserError (Maybe Expr))
    incrementOrBlockV = expr <|> junk <|> empty
      where
        empty = pure $ Success Nothing
        expr =
          do
            good <- expression
            pure $ Success $ Just good
        junk =
          do
            token <- one LeftBrace
            throwaway RightBrace
            pure $ Failure $ ParserError InvalidExpression token

ifStatement :: Parser Statement
ifStatement =
  do
    throwaway If
    require LeftParen
    antecedent   <- expressionOrCry
    require RightParen
    consequent   <- statementOrCry
    alternativeM <- optional $ do
      throwaway Else
      statementOrCry
    pure $ IfElse antecedent consequent alternativeM

whileStatement :: Parser Statement
whileStatement =
  do
    throwaway While
    require LeftParen
    cond <- expressionOrCry
    require RightParen
    body <- statementOrCry
    pure $ WhileStatement cond body

expressionOrCry :: Parser Expr
expressionOrCry = expression <|> cryAbout InvalidExpression

statementOrCry :: Parser Statement
statementOrCry  = statement <|> cryAbout InvalidExpression
