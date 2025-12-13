module Lox.Parser.Internal.Parser(parse) where

import Lox.Scanner.Token(TokenPlus)

import Lox.Parser.Internal.Nihilism(errorParser)
import Lox.Parser.Internal.Optimism(program)
import Lox.Parser.Internal.Parse(Parser(run))
import Lox.Parser.Internal.ParserError(ParserError)
import Lox.Parser.Internal.Program(Program)

import qualified Data.List.NonEmpty as NE


parse :: [TokenPlus] -> Validation (NonEmpty ParserError) Program
parse = parser.run &> (either handleError handleSuccess)
  where
    handleError   = NE.singleton &> Failure
    handleSuccess = fst &> Success

parser :: Parser Program
parser = program <|> errorParser
