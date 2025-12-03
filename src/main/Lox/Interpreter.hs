module Lox.Interpreter(interpret, Result(OtherFailure, ParserFailure, ScannerFailure, Success)) where

import Data.List.NonEmpty(NonEmpty)
import Data.Validation(validation)
import Data.Void(Void)

import Lox.Scanner.Scanner(scan)
import Lox.Scanner.ScannerError(ScannerError)

import Lox.Parser.Parser(parse)
import Lox.Parser.ParserError(ParserError)
import Lox.Parser.Program(Program)


data Result
  = ScannerFailure (NonEmpty ScannerError)
  | ParserFailure  (NonEmpty ParserError)
  | OtherFailure   (NonEmpty Void)
  | Success        Program

interpret :: Text -> Result
interpret = runM
  where
    runM   = scan  &> validation ScannerFailure parseM
    parseM = parse &> validation ParserFailure  Success
