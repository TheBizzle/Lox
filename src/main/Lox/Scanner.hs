{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Lox.Scanner(ParserError(typ, lineNumber, ParserError), ParserErrorType(UnknownToken, UnterminatedString), scan, Token, TokenPlus(lineNumber, token, TokenPlus)) where

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
      scanToken
      scan_
    else do
      modify $ \s -> s { tokens = s.tokens ++ [TokenPlus EOF s.sLineNumber] }
      state <- get
      let errorsMaybe = nonEmpty state.errors
      return $ maybe (_Success # state.tokens) (_Failure #) errorsMaybe

checkForEnd :: State ScannerState Bool
checkForEnd =
  do
    state <- get
    return $ state.current >= (Text.length state.source)

scanToken :: State ScannerState ()
scanToken =
  do
    char <- slurpNextChar
    case char of
      '('  -> addToken LeftParen
      ')'  -> addToken RightParen
      '{'  -> addToken LeftBrace
      '}'  -> addToken RightBrace
      ','  -> addToken Comma
      '.'  -> addToken Dot
      '-'  -> addToken Minus
      '+'  -> addToken Plus
      ';'  -> addToken Semicolon
      '*'  -> addToken Star
      '!'  -> ((matches '=') <&> (\c -> if c then BangEqual    else Bang   )) >>= addToken
      '='  -> ((matches '=') <&> (\c -> if c then EqualEqual   else Equal  )) >>= addToken
      '<'  -> ((matches '=') <&> (\c -> if c then LessEqual    else Less   )) >>= addToken
      '>'  -> ((matches '=') <&> (\c -> if c then GreaterEqual else Greater)) >>= addToken
      '/'  -> (matches '/') >>= (\c -> if c then skipToEOL else addToken Slash)
      ' '  -> skip
      '\t' -> skip
      '\r' -> skip
      '\n' -> (modify $ \s -> s { sLineNumber = s.sLineNumber + 1 }) >> skip
      '"'  -> slurpString
      x -> do
        let err = \lNum -> ParserError (UnknownToken $ asText [x]) lNum
        modify $ \s -> s { errors = s.errors ++ [err s.sLineNumber] }
  where
    skip = return ()

slurpNextChar :: State ScannerState Char
slurpNextChar =
  do
    state <- get
    let char = Text.index state.source state.current
    put (state { current = state.current + 1 })
    return char

addToken :: Token -> State ScannerState ()
addToken token =
  do
    state <- get
    let tplus = TokenPlus { token = token, lineNumber = state.sLineNumber }
    put (state { tokens = state.tokens ++ [tplus] })

matches :: Char -> State ScannerState Bool
matches c =
  do
    isAtEnd <- checkForEnd
    state   <- get
    let char      = Text.index state.source state.current
    let doesMatch = (not isAtEnd) && char == c
    when doesMatch $ modify $ \s -> s { current = s.current + 1 }
    return doesMatch

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

peek :: State ScannerState Char
peek =
  do
    isAtEnd <- checkForEnd
    state   <- get
    return $ if isAtEnd then '\0' else Text.index state.source state.current

slurpString :: State ScannerState ()
slurpString =
  do
    isAtEnd <- helper
    if not isAtEnd then do
      _     <- slurpNextChar -- Closing '"'
      state <- get
      let str = state.source |> Text.drop (state.start + 1) &> Text.take (state.current - state.start - 2)
      addToken $ String str
    else
      modify $ \s -> s { errors = s.errors ++ [ParserError UnterminatedString s.sLineNumber] }
  where
    helper =
      do
        isAtEnd <- checkForEnd
        c       <- peek
        if c /= '"' && (not isAtEnd) then do
          when (c == '\n') $ modify $ \s -> s { sLineNumber = s.sLineNumber + 1 }
          _ <- slurpNextChar
          helper
        else
          return isAtEnd

data ScannerState
  = SState { source :: Text, tokens :: [TokenPlus], errors :: [ParserError], current :: Int, start :: Int, sLineNumber :: Int }

type ParserResult a = Validation (NonEmpty ParserError) a

data ParserErrorType
  = UnknownToken Text
  | UnterminatedString

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
  show (String x)     = "\"" <> (Text.unpack x) <> "\""
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
