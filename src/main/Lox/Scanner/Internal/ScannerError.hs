module Lox.Scanner.Internal.ScannerError(
    ScannerErrorType(InvalidNumberFormat, UnknownToken, UnterminatedString)
  , ScannerError(lineNumber, ScannerError, typ)
  ) where

data ScannerErrorType
  = InvalidNumberFormat Text
  | UnknownToken Text
  | UnterminatedString

data ScannerError =
  ScannerError { typ :: ScannerErrorType, lineNumber :: Int }
