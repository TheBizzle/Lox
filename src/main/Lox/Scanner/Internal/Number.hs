module Lox.Scanner.Internal.Number(slurpNumber) where

import Control.Monad.State(State)

import Text.Read(readMaybe)

import Lox.Scanner.Internal.Classify(isDigit)
import Lox.Scanner.Internal.ScannerError(ScannerErrorType(InvalidNumberFormat))
import Lox.Scanner.Internal.ScannerState(addError, addToken, peek, peek2, ScannerState, slurpNextChar)
import Lox.Scanner.Internal.Token(Token(Number))

import qualified Data.List as List


slurpNumber :: Char -> State ScannerState ()
slurpNumber c =
  do
    integerCs <- helper [c]
    c         <- peek
    c2        <- peek2
    numRev    <-
      if (c == '.') && (isDigit c2) then do
        dot <- slurpNextChar
        helper $ dot : integerCs
      else
        return integerCs
    let numMaybe = (numRev |> List.reverse &> readMaybe) :: Maybe Double
    maybe (reportNumError numRev) (Number &> addToken) numMaybe
  where

    helper acc =
      do
        c <- peek
        if isDigit c then do
          next <- slurpNextChar
          helper $ next : acc
        else
          return acc

    reportNumError = List.reverse &> asText &> InvalidNumberFormat &> addError
