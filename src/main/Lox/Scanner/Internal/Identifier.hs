module Lox.Scanner.Internal.Identifier(slurpIdentifier) where

import Control.Monad.State(State)

import Lox.Scanner.Internal.Classify(isAlphanumeric)
import Lox.Scanner.Internal.ScannerState(addToken, peek, ScannerState, slurpNextChar)
import Lox.Scanner.Internal.Token(Token(And, Class, Else, For, Fun, Identifier, If, Nil, Or, Print, Return, Super, This, TokenFalse, TokenTrue, Var, While))

import qualified Data.List as List


slurpIdentifier :: Char -> State ScannerState ()
slurpIdentifier c = (identChars [c]) <&> (List.reverse &> asText &> identifierishToken) >>= addToken
  where
    identChars acc =
      do
        c <- peek
        if isAlphanumeric c then do
          next <- slurpNextChar
          identChars $ next : acc
        else
          return acc

identifierishToken :: Text -> Token
identifierishToken "and"    = And
identifierishToken "class"  = Class
identifierishToken "else"   = Else
identifierishToken "false"  = TokenFalse
identifierishToken "for"    = For
identifierishToken "fun"    = Fun
identifierishToken "if"     = If
identifierishToken "nil"    = Nil
identifierishToken "or"     = Or
identifierishToken "print"  = Print
identifierishToken "return" = Return
identifierishToken "super"  = Super
identifierishToken "this"   = This
identifierishToken "true"   = TokenTrue
identifierishToken "var"    = Var
identifierishToken "while"  = While
identifierishToken x        = Identifier x
