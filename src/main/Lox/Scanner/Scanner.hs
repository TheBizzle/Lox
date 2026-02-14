module Lox.Scanner.Scanner(scan) where

import Control.Monad.State(get, modify)

import Lox.Scanner.Token(
    SourceLoc(SourceLoc)
  , Token(Bang, BangEqual, Comma, Dot, Equal, EqualEqual, EOF, Greater, GreaterEqual, LeftBrace, LeftParen, Less, LessEqual, Minus, Plus, RightBrace, RightParen, Semicolon, Slash, Star)
  , TokenPlus(TokenPlus)
  )

import Lox.Scanner.Internal.Classify(isAlphabetic, isDigit)
import Lox.Scanner.Internal.Identifier(slurpIdentifier)
import Lox.Scanner.Internal.Number(slurpNumber)
import Lox.Scanner.Internal.ScannerError(ScannerError, ScannerErrorType(UnknownToken))
import Lox.Scanner.Internal.ScannerState(addToken, addError, checkForEnd, ScannerState(current, errors, ScannerState, lineNumber, start, tokens), skipToEOL, slurpMatch, slurpNextChar)
import Lox.Scanner.Internal.String(slurpString)

import qualified Data.List as List


type ScannerResult a = Validation (NonEmpty ScannerError) a

scan :: Text -> ScannerResult [TokenPlus]
scan code = (ScannerState code [] [] 0 0 1) |> (runState scan_) &> fst

scan_ :: State ScannerState (ScannerResult [TokenPlus])
scan_ =
  do
    isAtEnd <- checkForEnd
    if not isAtEnd then do
      modify $ \s -> s { start = s.current }
      scanToken
      scan_
    else do
      modify $ \s -> s { tokens = s.tokens <> [TokenPlus EOF $ SourceLoc s.lineNumber] }
      state <- get
      let errorsMaybe = state.errors |> List.reverse &> nonEmpty
      return $ maybe (Success state.tokens) Failure errorsMaybe

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
      '!'  -> ((slurpMatch '=') <&> (\c -> if c then BangEqual    else Bang   )) >>= addToken
      '='  -> ((slurpMatch '=') <&> (\c -> if c then EqualEqual   else Equal  )) >>= addToken
      '<'  -> ((slurpMatch '=') <&> (\c -> if c then LessEqual    else Less   )) >>= addToken
      '>'  -> ((slurpMatch '=') <&> (\c -> if c then GreaterEqual else Greater)) >>= addToken
      '/'  -> (slurpMatch '/') >>= (\c -> if c then skipToEOL else addToken Slash)
      ' '  -> skip
      '\t' -> skip
      '\r' -> skip
      '\n' -> (modify $ \s -> s { lineNumber = s.lineNumber + 1 }) >> skip
      '"'  -> slurpString
      x    -> scanTier2 x
  where
    skip = return ()

    scanTier2 x =
      if isDigit x then
        slurpNumber x
      else if isAlphabetic x then
        slurpIdentifier x
      else
        addError $ UnknownToken $ asText [x]
