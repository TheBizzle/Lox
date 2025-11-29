{-# LANGUAGE OverloadedRecordDot #-}

module Main(main) where

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(ExitFailure))

import Lox.Interpreter(interpret, Result(OtherFailure, ParserFailure, ScannerFailure, Success))
import Lox.Scanner.ScannerError(ScannerError(lineNumber, typ), ScannerErrorType(InvalidNumberFormat, UnknownToken, UnterminatedString))
import Lox.Scanner.Token(TokenPlus(token))

import qualified Data.Text.IO as TIO


main :: IO ()
main = getArgs >>= processArgs
  where
    processArgs :: [String] -> IO ()
    processArgs            [] = runPrompt
    processArgs (filePath:[]) = (TIO.readFile filePath) >>= runFile
    processArgs             _ = (TIO.putStrLn "Usage: lox [script]") >> (exitWith $ ExitFailure 64)

runPrompt :: IO ()
runPrompt =
  do
    putStrFlush "> "
    line <- TIO.getLine
    when (line /= "exit") $ (run line) >> runPrompt

runFile :: Text -> IO ()
runFile code =
  do
    wasSuccess <- run code
    when (not wasSuccess) $ exitWith $ ExitFailure 65

run :: Text -> IO Bool
run code =
  case (interpret code) of
    ScannerFailure errors -> handleError scannerErrorAsText errors
    ParserFailure  errors -> handleError          anyAsText errors
    OtherFailure   errors -> handleError          anyAsText errors
    Success        tokens -> (for_ tokens $ token &> showText &> TIO.putStrLn) $> True
  where
    handleError errorToText errors =
      (for_ errors $ errorToText &> TIO.putStrLn) $> False

anyAsText :: a -> Text
anyAsText _ = error "Unimplemented error handler"

scannerErrorAsText :: ScannerError -> Text
scannerErrorAsText error = line
  where
    line = "[line " <> (showText error.lineNumber) <> "] Error - " <> (errorText error.typ)
    errorText (InvalidNumberFormat c) = "Invalid number format: " <> c
    errorText (UnknownToken c)        = "Unknown token: " <> c
    errorText UnterminatedString      = "Unterminated string"
