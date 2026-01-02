module Lox.Interpreter(interpret, Result(OtherFailure, ParserFailure, ScannerFailure, Succeeded)) where

import Lox.Scanner.Scanner(scan)
import Lox.Scanner.ScannerError(ScannerError)

import Lox.Parser.Parser(parse)
import Lox.Parser.ParserError(ParserError)

import Lox.Evaluator.Evaluator(eval)
import Lox.Evaluator.EvalError(EvalError)
import Lox.Evaluator.Program(Program)
import Lox.Evaluator.Value(Value)


data Result
  = ScannerFailure (NonEmpty ScannerError)
  | ParserFailure  (NonEmpty ParserError)
  | OtherFailure   (NonEmpty Void)
  | Succeeded      (Program (Validation (NonEmpty EvalError) Value))

interpret :: Text -> Result
interpret = runM
  where
    runM   = scan  &> validation ScannerFailure parseM
    parseM = parse &> validation ParserFailure  evalM
    evalM  = eval  &> Succeeded
