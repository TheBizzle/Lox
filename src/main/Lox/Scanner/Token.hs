{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Lox.Scanner.Token(module Lox.Scanner.Internal.Token) where

import Lox.Scanner.Internal.Token(
    SourceLoc(lineNumber, SourceLoc)
  , Token(loc, Token, typ)
  , TokenType(..)
  )
