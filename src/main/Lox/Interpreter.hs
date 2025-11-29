module Lox.Interpreter(interpret, Result(OtherFailure, ParserFailure, ScannerFailure, Success)) where

import Data.List.NonEmpty(NonEmpty)
import Data.Validation(validation)
import Data.Void(Void)

import Lox.Scanner.Scanner(scan)
import Lox.Scanner.ScannerError(ScannerError)
import Lox.Scanner.Token(TokenPlus)


data Result
  = ScannerFailure (NonEmpty ScannerError)
  | ParserFailure  (NonEmpty Void)
  | OtherFailure   (NonEmpty Void)
  | Success        [TokenPlus]

interpret :: Text -> Result
interpret code = result
  where
    scanV  = scan code
    result = validation ScannerFailure Success scanV
