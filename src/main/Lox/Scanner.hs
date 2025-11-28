{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Lox.Scanner(ParserError(typ, lineNumber, ParserError), ParserErrorType(UnknownToken), scan, Token, TokenPlus(lineNumber, token, TokenPlus)) where

import Control.Lens((#))
import Control.Monad.State(get, modify, put, runState, State)

import Data.List((++))
import Data.List.NonEmpty(nonEmpty, NonEmpty)
import Data.Validation(_Failure, _Success, Validation)

import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as Text

scan :: Text -> ParserResult [TokenPlus]
scan code = (SState code [] [] 0 0 1) |> (runState scan_) &> fst

scan_ :: State ScannerState (ParserResult [TokenPlus])
scan_ =
  do
    isAtEnd <- checkForEnd
    if not isAtEnd then do
      modify $ \s -> s { start = s.current }
      result <- scanToken
      forM_ result $ \e -> modify $ \s -> s { errors = s.errors ++ [e] }
      scan_
    else do
      modify $ \s -> s { tokens = s.tokens ++ [TokenPlus EOF s.lineNumber] }
      state <- get
      let errorsMaybe = nonEmpty state.errors
      return $ maybe (_Success # state.tokens) (_Failure #) errorsMaybe

checkForEnd :: State ScannerState Bool
checkForEnd =
  do
    state <- get
    return $ state.current >= (Text.length state.source)

scanToken :: State ScannerState (Maybe ParserError)
scanToken =
  do
    char <- slurpNextChar
    case char of
      '(' -> safelyAddToken LeftParen
      ')' -> safelyAddToken RightParen
      '{' -> safelyAddToken LeftBrace
      '}' -> safelyAddToken RightBrace
      ',' -> safelyAddToken Comma
      '.' -> safelyAddToken Dot
      '-' -> safelyAddToken Minus
      '+' -> safelyAddToken Plus
      ';' -> safelyAddToken Semicolon
      '*' -> safelyAddToken Star
      x -> do
        state <- get
        return $ Just $ ParserError (UnknownToken $ asText [x]) state.lineNumber

slurpNextChar :: State ScannerState Char
slurpNextChar =
  do
    state <- get
    let char = Text.index state.source state.current
    put (state { current = state.current + 1 })
    return char

safelyAddToken :: Token -> State ScannerState (Maybe ParserError)
safelyAddToken token = (addToken token) $> Nothing

addToken :: Token -> State ScannerState ()
addToken token =
  do
    state <- get
    let tplus = TokenPlus { token = token, lineNumber = state.lineNumber }
    put (state { tokens = state.tokens ++ [tplus] })

data ScannerState
  = SState { source :: Text, tokens :: [TokenPlus], errors :: [ParserError], current :: Int, start :: Int, lineNumber :: Int }

type ParserResult a = Validation (NonEmpty ParserError) a

data ParserErrorType
  = UnknownToken Text

data ParserError =
  ParserError { typ :: ParserErrorType, lineNumber :: Int }

data TokenPlus =
  TokenPlus { token :: Token, lineNumber :: Int }

data Token
  = LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Identifier Text
  | String Text
  | Number Text
  | And
  | Class
  | Else
  | TokenFalse
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | TokenTrue
  | Var
  | While
  | EOF

instance Show Token where
  show LeftParen      = "("
  show RightParen     = ")"
  show LeftBrace      = "{"
  show RightBrace     = "}"
  show Comma          = ","
  show Dot            = "."
  show Minus          = "-"
  show Plus           = "+"
  show Semicolon      = ";"
  show Slash          = "/"
  show Star           = "*"
  show Bang           = "!"
  show BangEqual      = "!="
  show Equal          = "="
  show EqualEqual     = "=="
  show Greater        = ">"
  show GreaterEqual   = ">="
  show Less           = "<"
  show LessEqual      = "<="
  show (Identifier x) = "(identifier: " <> (Text.unpack x) <> ")"
  show (String x)     = "(string: "     <> (Text.unpack x) <> ")"
  show (Number x)     = "(number: "     <> (Text.unpack x) <> ")"
  show And            = "and"
  show Class          = "class"
  show Else           = "else"
  show TokenFalse     = "false"
  show Fun            = "function"
  show For            = "for"
  show If             = "if"
  show Nil            = "nil"
  show Or             = "||"
  show Print          = "print"
  show Return         = "return"
  show Super          = "super"
  show This           = "this"
  show TokenTrue      = "true"
  show Var            = "var"
  show While          = "while"
  show EOF            = "\\EOF"
