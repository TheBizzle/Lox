{-# LANGUAGE OverloadedRecordDot #-}

module Lox.Scanner.Internal.ScannerState(addError, addToken, checkForEnd, peek, peek2, ScannerState(current, errors, lineNumber, ScannerState, source, start, tokens), skipToEOL, slurpMatch, slurpNextChar) where

import Control.Applicative(liftA2)
import Control.Monad.State(get, modify, put, State)

import Data.List((++))

import Lox.Scanner.Internal.ScannerError(ScannerError(ScannerError), ScannerErrorType)
import Lox.Scanner.Internal.Token(Token, TokenPlus(token, TokenPlus))

import qualified Lox.Scanner.Internal.Token as Token
import qualified Data.Text                  as Text


data ScannerState
  = ScannerState { source     :: Text
                 , tokens     :: [TokenPlus]
                 , errors     :: [ScannerError]
                 , current    :: Int
                 , start      :: Int
                 , lineNumber :: Int
                 }

(<*@>) :: Applicative f => f a -> f b -> f (a, b)
(<*@>) = liftA2 (,)

addToken :: Token -> State ScannerState ()
addToken token =
  do
    state <- get
    let tplus = TokenPlus { token = token, Token.lineNumber = state.lineNumber }
    put (state { tokens = state.tokens ++ [tplus] })

addError :: ScannerErrorType -> State ScannerState ()
addError errorType =
  modify $ \s -> s { errors = (ScannerError errorType s.lineNumber) : s.errors }

checkForEnd :: State ScannerState Bool
checkForEnd = get <&> \state -> state.current >= (Text.length state.source)

slurpMatch :: Char -> State ScannerState Bool
slurpMatch c =
  do
    isAtEnd <- checkForEnd
    state   <- get
    let char      = Text.index state.source state.current
    let doesMatch = (not isAtEnd) && char == c
    when doesMatch $ modify $ \s -> s { current = s.current + 1 }
    return doesMatch

peek :: State ScannerState Char
peek = (get <*@> checkForEnd) <&> \(state, isAtEnd) ->
  if not isAtEnd then
    Text.index state.source state.current
  else
    '\0'

peek2 :: State ScannerState Char
peek2 = get <&> \state ->
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
