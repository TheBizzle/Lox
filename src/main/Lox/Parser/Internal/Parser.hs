module Lox.Parser.Internal.Parser(parse) where

import Lox.Scanner.Token(TokenPlus)

import Lox.Parser.Internal.Nihilism(errorParser)
import Lox.Parser.Internal.Optimism(ast)
import Lox.Parser.Internal.Parse(Parser(run))
import Lox.Parser.Internal.ParserError(ParserError)
import Lox.Parser.Internal.AST(AST)


parse :: [TokenPlus] -> Validation (NonEmpty ParserError) AST
parse = parser.run &> (either handleError handleSuccess)
  where
    handleError   = Failure
    handleSuccess = fst &> Success

parser :: Parser AST
parser = ast <|> errorParser
