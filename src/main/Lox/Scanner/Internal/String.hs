{-# LANGUAGE OverloadedRecordDot #-}

module Lox.Scanner.Internal.String(slurpString) where

import Control.Monad.State(modify, State)

import Lox.Scanner.Internal.ParserError(ParserErrorType(UnterminatedString))
import Lox.Scanner.Internal.ScannerState(addError, addToken, checkForEnd, peek, ScannerState(sLineNumber), slurpNextChar)
import Lox.Scanner.Internal.Token(Token(String))

import qualified Data.List as List


slurpString :: State ScannerState ()
slurpString =
  do
    (isAtEnd, chars) <- helper []
    if not isAtEnd then do
      _ <- slurpNextChar -- Closing '"'
      chars |> List.reverse &> asText &> String &> addToken
    else
      addError UnterminatedString
  where
    helper acc =
      do
        isAtEnd <- checkForEnd
        c       <- peek
        if c /= '"' && (not isAtEnd) then do
          when (c == '\n') $ modify $ \s -> s { sLineNumber = s.sLineNumber + 1 }
          _ <- slurpNextChar
          helper (c : acc)
        else
          return (isAtEnd, acc)
