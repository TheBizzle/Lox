module Lox.Scanner.Internal.ScannerError(
    ScannerError(lineNumber, ScannerError, typ)
  , ScannerErrorType(InvalidNumberFormat, UnknownToken, UnterminatedString)
  ) where

data ScannerErrorType
  = InvalidNumberFormat Text
  | UnknownToken        Text
  | UnterminatedString

data ScannerError =
  ScannerError { typ :: ScannerErrorType, lineNumber :: Word }
