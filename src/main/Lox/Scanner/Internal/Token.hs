module Lox.Scanner.Internal.Token(Token(..), TokenPlus(lineNumber, token, TokenPlus)) where

import qualified Data.Text as Text


data TokenPlus =
  TokenPlus { token :: Token, lineNumber :: Word }

instance Show TokenPlus where
  show (TokenPlus t ln) = (show t) <> "_L" <> (show ln)

data Token
  = And
  | Bang
  | BangEqual
  | Class
  | Comma
  | Dot
  | Else
  | EOF
  | Equal
  | EqualEqual
  | For
  | Fun
  | Greater
  | GreaterEqual
  | Identifier Text
  | If
  | LeftBrace
  | LeftParen
  | Less
  | LessEqual
  | Minus
  | Nil
  | Number Double
  | Or
  | Plus
  | Print
  | Return
  | RightBrace
  | RightParen
  | Semicolon
  | Slash
  | Star
  | String Text
  | Super
  | This
  | TokenFalse
  | TokenTrue
  | Var
  | While
  deriving Eq

instance Show Token where
  show And            = "&&"
  show Bang           = "!"
  show BangEqual      = "!="
  show Class          = "class"
  show Comma          = ","
  show Dot            = "."
  show EOF            = "\\EOF"
  show Else           = "else"
  show Equal          = "="
  show EqualEqual     = "=="
  show For            = "for"
  show Fun            = "function"
  show Greater        = ">"
  show GreaterEqual   = ">="
  show (Identifier x) = "(identifier: " <> asString x <> ")"
  show If             = "if"
  show LeftBrace      = "{"
  show LeftParen      = "("
  show Less           = "<"
  show LessEqual      = "<="
  show Minus          = "-"
  show Nil            = "nil"
  show (Number x)     = x |> showText &> ((id &&& Text.stripSuffix ".0") &> (\(a, b) -> maybe a id b)) &> asString
  show Or             = "||"
  show Plus           = "+"
  show Print          = "print"
  show Return         = "return"
  show RightBrace     = "}"
  show RightParen     = ")"
  show Semicolon      = ";"
  show Slash          = "/"
  show Star           = "*"
  show (String x)     = "\"" <> asString x <> "\""
  show Super          = "super"
  show This           = "this"
  show TokenFalse     = "false"
  show TokenTrue      = "true"
  show Var            = "var"
  show While          = "while"
