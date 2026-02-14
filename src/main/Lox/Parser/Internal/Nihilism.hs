module Lox.Parser.Internal.Nihilism(errorParser) where

import Lox.Scanner.Token(
    Token(And, Bang, BangEqual, Else, EOF, Equal, EqualEqual, For, Greater, GreaterEqual, If, LeftBrace, LeftParen, Less, LessEqual, Minus, Or, Plus, Print, Return, RightBrace, RightParen, Semicolon, Slash, Star, Var, While)
  , TokenPlus(loc, token, TokenPlus)
  )

import Lox.Parser.Internal.AST(exprToToken)

import Lox.Parser.Internal.ExpressionParser(expression, unary)
import Lox.Parser.Internal.Optimism(declaration)
import Lox.Parser.Internal.Parse(errorWith, keywords, oneOf, Parser, parserFrom, throwaway, variable)

import Lox.Parser.Internal.ParserError(
    ErrorPriority(Unimportant, VeryHigh)
  , ParserError(ParserError)
  , ParserErrorType(Backtrack, ExpectedIdentifier, InvalidExpression, Missing)
  )


errorParser :: Parser a
errorParser = (many declaration) *> badDeclaration

badDeclaration :: Parser a
badDeclaration = badVarDecl1 <|> badVarDecl2 <|> badVarDecl3 <|> badVarDecl4 <|> badVarDecl5 <|>
                 badStatement
  where
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
                 badIfElse <|> badIf <|>
                 badWhile <|>
                 badReturn <|>
                 badBlock1 <|> badBlock2 <|> badBlock3 <|> badBlock4 <|>
                 badExprStatement1 <|> badExprStatement2
  where

    badPrintStatement1 = (throwaway Print) *> expression *> (whine $ Missing Semicolon)
    badPrintStatement2 = (throwaway Print) *> badExpression

    badFor = (throwaway For) *> (whineIf EOF $ Missing EOF) -- TODO

    badIfElse = (throwaway Else) *> (whineIf EOF $ Missing EOF) -- TODO
    badIf     = (throwaway If  ) *> (whineIf EOF $ Missing EOF) -- TODO

    badWhile = (throwaway While) *> (whineIf EOF $ Missing EOF) -- TODO

    badReturn = (throwaway Return) *> (whineIf EOF $ Missing EOF) -- TODO

    badBlock1 = (throwaway LeftBrace) *> (many declaration) *> badDeclaration
    badBlock2 = (throwaway LeftBrace) *> (many declaration) *> (whineIfNot RightBrace)
    badBlock3 = declaration *> (whineIf RightBrace $ Missing LeftBrace)
    badBlock4 = whineIf RightBrace $ Missing LeftBrace

    badExprStatement1 = expression *> (whine $ Missing Semicolon)
    badExprStatement2 = badExpression

badExpression :: Parser a
badExpression = badAssignment

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
badUnary = (many $ oneOf [Bang, Minus]) *> badPrimary

badPrimary :: Parser a
badPrimary = badGrouping1 <|> badGrouping2 <|> badGrouping3 <|> badGrouping4 <|> badGrouping5 <|> badFnCall <|> (whine InvalidExpression)
  where
    badFnCall    = (throwaway For) *> (whineIf EOF $ Missing EOF) -- TODO
    badGrouping1 = (throwaway LeftParen) *> (whineIf RightParen InvalidExpression)
    badGrouping2 = (throwaway LeftParen) *> expression *> (whineIfNot RightParen)
    badGrouping3 = (throwaway LeftParen) *> badExpression
    badGrouping4 = expression *> (whineIf RightParen $ Missing LeftParen)
    badGrouping5 = whineIf RightParen $ Missing LeftParen

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
