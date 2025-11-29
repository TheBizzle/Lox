module Lox.Scanner.Internal.Token(Token(..), TokenPlus(lineNumber, token, TokenPlus)) where

import qualified Data.Text as Text


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
  | Number Double
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
  show (Identifier x) = "(identifier: " <> asString x <> ")"
  show (String x)     = "\"" <> asString x <> "\""
  show (Number x)     = x |> showText &> ((id &&& Text.stripSuffix ".0") &> (\(a, b) -> maybe a id b)) &> asString
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
