module Lox.Parser.Internal.Nihilism(errorParser) where

import Control.Monad(replicateM)

import Lox.Scanner.Token(
    Token(And, Bang, BangEqual, Class, Comma, Dot, Else, EOF, Equal, EqualEqual, For, Fun, Greater, GreaterEqual, If, LeftBrace, LeftParen, Less, LessEqual, Minus, Or, Plus, Print, Return, RightBrace, RightParen, Semicolon, Slash, Star, Super, Var, While)
  , TokenPlus(loc, token, TokenPlus)
  )

import Lox.Parser.Internal.AST(exprToToken)

import Lox.Parser.Internal.ExpressionParser(expression, primary, unary)
import Lox.Parser.Internal.Optimism(declaration, fnParams, function, statement)
import Lox.Parser.Internal.Parse(errorWith, keywords, notFollowedBy, one, oneOf, Parser, parserFrom, throwaway, variable)

import Lox.Parser.Internal.ParserError(
    ErrorPriority(Unimportant, VeryHigh)
  , ParserError(ParserError)
  , ParserErrorType(Backtrack, ExpectedDotAfterSuper, ExpectedIdentifier, ExpectedSuperMethodName, ExpectedSuperName, InvalidExpression, Missing, TooMuchArguing, TooMuchParaming)
  )


errorParser :: Parser a
errorParser = (many declaration) *> badDeclaration

badDeclaration :: Parser a
badDeclaration = badClass1 <|> badClass2 <|>
                   badFunction1 <|>
                   badVarDecl1 <|> badVarDecl2 <|> badVarDecl3 <|> badVarDecl4 <|> badVarDecl5 <|>
                   badStatement
  where
    badClass1 =
      (throwaway Class) *>
        variable *>
        (throwaway Less) *>
        (notFollowedBy variable) *>
        (whine ExpectedSuperName)

    badClass2 =
      (throwaway Class) *>
        variable *>
        (optional $ (throwaway Less) *> variable) *>
        (throwaway LeftBrace) *>
        (many function) *>
        (badMethod1 <|> badMethod2)

    badMethod1 = variable *> oversizedParamList *> whine TooMuchParaming
    badMethod2 = variable *> (throwaway LeftParen) *> fnParams *> (throwaway RightParen) *> badBlock

    badFunction1 = (throwaway Fun) *> variable *> oversizedParamList *> whine TooMuchParaming

    -- bvd1 catches `var x = !3 == (3) var` | bvd2 catches `var x = !3 ==` --Jason B. (12/12/25)
    badVarDecl1 = (throwaway Var) *> variable *> (throwaway Equal) *> expression *> detectCompound
    badVarDecl2 = (throwaway Var) *> variable *> (throwaway Equal) *> badExpression
    badVarDecl3 = (throwaway Var) *> variable *> (whineParsed expression exprToToken $ Missing Equal)
    badVarDecl4 = (throwaway Var) *> variable *> (whine $ Missing Semicolon)
    badVarDecl5 = (throwaway Var) *> (whine ExpectedIdentifier)

    detectCompound = whineIfEOF <|> whineIfEndBlock <|> whineIfKeyword
      where
        whineIfEOF      = whineIf EOF                     $ Missing Semicolon
        whineIfEndBlock = whineIf RightBrace              $ Missing Semicolon
        whineIfKeyword  = whineParsed (oneOf keywords) id $ Missing Semicolon

badStatement :: Parser a
badStatement = badPrintStatement1 <|> badPrintStatement2 <|>
                 badFor <|>
                 badIfElse <|>
                 badIf <|>
                 badWhile <|>
                 badReturn <|>
                 badBlock <|>
                 badExprStatement1 <|> badExprStatement2 <|>
                 dangler1 <|> dangler2
  where
    badPrintStatement1 = (throwaway Print) *> expression *> (whine $ Missing Semicolon)
    badPrintStatement2 = (throwaway Print) *> badExpression

    badFor = (throwaway For) *> (whineIf EOF $ Missing EOF) -- TODO

    badWhile = (throwaway While) *> (whineIf EOF $ Missing EOF) -- TODO

    badReturn = (throwaway Return) *> (whineIf EOF $ Missing EOF) -- TODO

    badExprStatement1 = badExpression
    badExprStatement2 = expression *> (whine $ Missing Semicolon)

    dangler1 = declaration *> (whineIf RightBrace $ Missing LeftBrace)
    dangler2 = whineIf RightBrace $ Missing LeftBrace

badIfElse :: Parser a
badIfElse =
  (throwaway If) *>
    (throwaway LeftParen) *>
    expression *>
    (throwaway RightParen) *>
    statement *>
    (throwaway Else) *>
    (notFollowedBy statement) *>
    (whine InvalidExpression)

badIf :: Parser a
badIf =
  (throwaway If) *>
    (throwaway LeftParen) *>
    expression *>
    (throwaway RightParen) *>
    (notFollowedBy statement) *>
    (whine InvalidExpression)

badBlock :: Parser a
badBlock = badBlock1 <|> badBlock2
  where
    badBlock1 = (throwaway LeftBrace) *> (many declaration) *> badDeclaration
    badBlock2 = (throwaway LeftBrace) *> (many declaration) *> (whineIfNot RightBrace)

badExpression :: Parser a
badExpression = badSuperApp1 <|> badSuperApp2 <|> badAssignment
  where
    badSuperApp1 =
      (throwaway Super) *>
        (throwaway Dot) *>
        (notFollowedBy variable) *>
        (whine ExpectedSuperMethodName)

badSuperApp2 :: Parser a
badSuperApp2 =
  (throwaway Super) *>
    (notFollowedBy $ one Dot) *>
    (whine ExpectedDotAfterSuper)

badAssignment :: Parser a
badAssignment = (many goodAss) *> (badAss <|> badBinary)
  where
    goodAss = variable *> (throwaway Equal)
    badAss  = whineParsed (expression <* (throwaway Equal)) exprToToken ExpectedIdentifier

badBinary :: Parser a
badBinary = (optional $ unary *> (many goodBinary) *> (oneOf binaryOperators)) *> badUnary
  where
    goodBinary      = (oneOf binaryOperators) *> expression
    binaryOperators = [And, BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Or, Plus, Slash, Star]

badUnary :: Parser a
badUnary = (many $ oneOf [Bang, Minus]) *> badFnCall

badFnCall :: Parser a
badFnCall = badFunApp <|> badPrimary
  where
    badFunApp :: Parser a
    badFunApp =
      primary *>
        (many $ (throwaway Dot) *> variable) <* (notFollowedBy $ one Equal) *>
        oversizedParamList *>
        whine TooMuchArguing

badPrimary :: Parser a
badPrimary = badGrouping1 <|> badGrouping2 <|> badGrouping3 <|> badGrouping4 <|> badGrouping5 <|> badGrouping6 <|>
               (whine InvalidExpression)
  where
    badGrouping1 = (throwaway LeftParen) *> (whineIf RightParen InvalidExpression)
    badGrouping2 = (throwaway LeftParen) *> badSuperApp2
    badGrouping3 = (throwaway LeftParen) *> expression *> (whineIfNot RightParen)
    badGrouping4 = (throwaway LeftParen) *> badExpression
    badGrouping5 = expression *> (whineIf RightParen $ Missing LeftParen)
    badGrouping6 = whineIf RightParen $ Missing LeftParen

oversizedParamList :: Parser ()
oversizedParamList =
  (throwaway LeftParen) *>
    variable *>
    (replicateM 254 $ (throwaway Comma) *> variable) *>
    (throwaway Comma)

whine :: ParserErrorType -> Parser a
whine = whineWithPrio VeryHigh

whineParsed :: Parser a -> (a -> TokenPlus) -> ParserErrorType -> Parser b
whineParsed parser toToken typ = parser >>= (toToken &> ParserError typ VeryHigh &> errorWith &> const &> parserFrom)

whineIf :: Token -> ParserErrorType -> Parser a
whineIf token typ = parserFrom helper
  where
    helper tp@(TokenPlus t _)
      | token == t = errorWith $ ParserError typ       VeryHigh    tp
      | otherwise  = errorWith $ ParserError Backtrack Unimportant tp

whineIfNot :: Token -> Parser a
whineIfNot token = parserFrom helper
  where
    helper tp@(TokenPlus t _)
      | token /= t = errorWith $ ParserError (Missing token) VeryHigh    tp
      | otherwise  = errorWith $ ParserError Backtrack       Unimportant tp

whineWithPrio :: ErrorPriority -> ParserErrorType -> Parser a
whineWithPrio prio typ = parserFrom $ \t -> errorWith $ ParserError typ prio $ TokenPlus t.token t.loc
