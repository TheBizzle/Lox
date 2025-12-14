module Lox.Parser.Internal.Nihilism(errorParser) where

import Lox.Scanner.Token(
    Token(Bang, BangEqual, EOF, Equal, EqualEqual, Greater, GreaterEqual, LeftParen, Less, LessEqual, Minus, Plus, Print, RightParen, Semicolon, Slash, Star, Var)
  , TokenPlus(lineNumber, token, TokenPlus)
  )

import Lox.Parser.Internal.ExpressionParser(expression, unary)
import Lox.Parser.Internal.Optimism(declaration)
import Lox.Parser.Internal.Parse(errorWith, keywords, oneOf, Parser, parserFrom, throwaway, variable)

import Lox.Parser.Internal.ParserError(
    ErrorPriority(Unimportant, VeryHigh)
  , ParserError(ParserError)
  , ParserErrorType(Backtrack, ExpectedIdentifier, InvalidExpression, Missing)
  )

import Lox.Parser.Internal.Program(exprToToken)


errorParser :: Parser a
errorParser = ((many declaration) *> badDeclaration)

badDeclaration :: Parser a
badDeclaration = badVarDecl1 <|> badVarDecl2 <|> badVarDecl3 <|> badVarDecl4 <|> badVarDecl5 <|>
                 badExprStatement1 <|> badExprStatement2 <|> badPrintStatement1 <|> badPrintStatement2
  where
    -- bvd1 catches `var x = !3 == (3) var` | bvd2 catches `var x = !3 ==` --Jason B. (12/12/25)
    badVarDecl1 = (throwaway Var) *> variable *> (throwaway Equal) *> expression *> detectCompound
    badVarDecl2 = (throwaway Var) *> variable *> (throwaway Equal) *> badExpression
    badVarDecl3 = (throwaway Var) *> variable *> (whineParsed expression exprToToken $ Missing Equal)
    badVarDecl4 = (throwaway Var) *> variable *> (whine $ Missing Semicolon)
    badVarDecl5 = (throwaway Var) *> (whine ExpectedIdentifier)

    badExprStatement1 = expression *> (whine $ Missing Semicolon)
    badExprStatement2 = badExpression

    badPrintStatement1 = (throwaway Print) *> expression *> (whine $ Missing Semicolon)
    badPrintStatement2 = (throwaway Print) *> badExpression

    detectCompound = whineIfEOF <|> whineIfKeyword
      where
        whineIfEOF     = whineIf EOF $ Missing Semicolon
        whineIfKeyword = whineParsed (oneOf keywords) id $ Missing Semicolon

badExpression :: Parser a
badExpression = badAssignment

badAssignment :: Parser a
badAssignment = (many goodAss) *> (badAss <|> badBinary)
  where
    goodAss = variable *> (throwaway Equal)
    badAss  = (whineParsed expression exprToToken ExpectedIdentifier) <* (throwaway Equal)

badBinary :: Parser a
badBinary = (optional $ unary *> (many goodBinary) *> (oneOf binaryOperators)) *> badUnary
  where
    goodBinary      = (oneOf binaryOperators) *> expression
    binaryOperators = [BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Plus, Slash, Star]

badUnary :: Parser a
badUnary = (many $ oneOf [Bang, Minus]) *> badPrimary

badPrimary :: Parser a
badPrimary = badGrouping1 <|> badGrouping2 <|> (whine InvalidExpression)
  where
    badGrouping1 = (throwaway LeftParen) *> expression *> (whine $ Missing RightParen)
    badGrouping2 = (throwaway LeftParen) *> badExpression

whine :: ParserErrorType -> Parser a
whine = whineWithPrio VeryHigh

whineParsed :: Parser a -> (a -> TokenPlus) -> ParserErrorType -> Parser b
whineParsed parser toToken typ = parser >>= helper
  where
    helper expr = parserFrom $ const $ errorWith $ ParserError typ VeryHigh tplus.lineNumber tplus.token
      where
        tplus = toToken expr

whineIf :: Token -> ParserErrorType -> Parser a
whineIf token typ = parserFrom helper
  where
    helper tp@(TokenPlus t _) = if token == t then
                                  errorWith $ ParserError typ VeryHigh tp.lineNumber tp.token
                                else
                                  errorWith $ ParserError Backtrack Unimportant tp.lineNumber tp.token

whineIfNot :: Token -> Parser a
whineIfNot token = parserFrom helper
  where
    helper tp@(TokenPlus t _) = if token /= t then
                                  errorWith $ ParserError (Missing token) VeryHigh tp.lineNumber tp.token
                                else
                                  errorWith $ ParserError Backtrack Unimportant tp.lineNumber tp.token


whineWithPrio :: ErrorPriority -> ParserErrorType -> Parser a
whineWithPrio prio typ = parserFrom $ \t -> errorWith $ ParserError typ prio t.lineNumber t.token
