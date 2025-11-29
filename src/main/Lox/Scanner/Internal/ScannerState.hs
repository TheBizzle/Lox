{-# LANGUAGE OverloadedRecordDot #-}

module Lox.Scanner.Internal.ScannerState(addError, addToken, checkForEnd, matches, peek, peek2, ScannerState(current, errors, ScannerState, sLineNumber, source, start, tokens), skipToEOL, slurpNextChar) where

import Control.Monad.State(get, modify, put, State)

import Data.List((++))

import Lox.Scanner.Internal.ParserError(ParserError(ParserError), ParserErrorType)
import Lox.Scanner.Internal.Token(Token, TokenPlus(lineNumber, token, TokenPlus))

import qualified Data.Text as Text


data ScannerState
  = ScannerState { source      :: Text
                 , tokens      :: [TokenPlus]
                 , errors      :: [ParserError]
                 , current     :: Int
                 , start       :: Int
                 , sLineNumber :: Int
                 }

addToken :: Token -> State ScannerState ()
addToken token =
  do
    state <- get
    let tplus = TokenPlus { token = token, lineNumber = state.sLineNumber }
    put (state { tokens = state.tokens ++ [tplus] })

addError :: ParserErrorType -> State ScannerState ()
addError errorType =
  modify $ \s -> s { errors = (ParserError errorType s.sLineNumber) : s.errors }

checkForEnd :: State ScannerState Bool
checkForEnd =
  do
    state <- get
    return $ state.current >= (Text.length state.source)

matches :: Char -> State ScannerState Bool
matches c =
  do
    isAtEnd <- checkForEnd
    state   <- get
    let char      = Text.index state.source state.current
    let doesMatch = (not isAtEnd) && char == c
    when doesMatch $ modify $ \s -> s { current = s.current + 1 }
    return doesMatch

peek :: State ScannerState Char
peek =
  do
    isAtEnd <- checkForEnd
    state   <- get
    return $ if not isAtEnd then Text.index state.source state.current else '\0'

peek2 :: State ScannerState Char
peek2 =
  do
    state <- get
    return $
      if (state.current + 1) < (Text.length state.source) then
        Text.index state.source $ state.current + 1
      else
        '\0'

skipToEOL :: State ScannerState ()
skipToEOL =
  do
    c       <- peek
    isAtEnd <- checkForEnd
    if c /= '\n' && (not isAtEnd) then do
      _ <- slurpNextChar
      skipToEOL
    else
      return ()

slurpNextChar :: State ScannerState Char
slurpNextChar =
  do
    state <- get
    let char = Text.index state.source state.current
    put $ state { current = state.current + 1 }
    return char
