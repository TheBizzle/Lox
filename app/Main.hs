{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main(main) where

import Control.Monad.State(runState)

import Data.List.NonEmpty(NonEmpty)
import Data.Traversable(Traversable)
import Data.Validation(validation, Validation)

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(ExitFailure))

import Lox.Evaluator.EvalError(EvalError(NotImplemented, TypeError))
import Lox.Evaluator.World(World, WorldState(WorldState))
import Lox.Evaluator.Value(Value)
import Lox.Interpreter(interpret, Result(OtherFailure, ParserFailure, ScannerFailure, Success))
import Lox.Parser.ParserError(ParserError(lineNumber, offender, typ), ParserErrorType(InvalidExpression, MissingClosingParen))
import Lox.Scanner.ScannerError(ScannerError(lineNumber, typ), ScannerErrorType(InvalidNumberFormat, UnknownToken, UnterminatedString))
import Lox.Scanner.Token(Token(EOF), TokenPlus(lineNumber, token))

import qualified Data.Map     as Map
import qualified Data.Text    as Text
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
runFile = run >=> \errorCode -> when (errorCode /= 0) $ exitWith $ ExitFailure errorCode

run :: Text -> IO Int
run code =
  case (interpret code) of
    ScannerFailure errors -> (handleError scannerErrorAsText errors) $> 65
    ParserFailure  errors -> (handleError  parserErrorAsText errors) $> 65
    OtherFailure   errors -> (handleError          anyAsText errors) $> 65
    Success        expr   -> runWorld expr

handleError :: Traversable t => (a -> Text) -> t a -> IO ()
handleError errorToText errors = for_ errors $ errorToText &> TIO.putStrLn

runWorld :: World (Validation (NonEmpty EvalError) Value) -> IO Int
runWorld world = output
  where
    (resultV, _) = runState world $ WorldState Map.empty []
    output       = validation handleBad handleGood resultV
    handleBad    = (handleError evalErrorAsText) &> ($> 70)
    handleGood   = showText &> TIO.putStrLn      &> ($> 0)

anyAsText :: a -> Text
anyAsText _ = error "Unimplemented error handler"

evalErrorAsText :: EvalError -> Text
evalErrorAsText = errorText
  where
    lineNum tp = showText tp.lineNumber
    errorText (NotImplemented tp) = "Runtime error (on line " <> (lineNum tp) <> "): Functionality not yet implemented"
    errorText (TypeError t pairs) = Text.intercalate "\n" $ map (typeError t) pairs
      where
        typeError tp (typ, value) = "Type error (on line " <> (lineNum tp) <> "): `" <> (showText tp.token) <>
          "` expected a value of type '" <> (showText typ) <> "', but got `" <> (showText value) <> "`"

parserErrorAsText :: ParserError -> Text
parserErrorAsText error = line
  where
    line = "[line " <> (showText error.lineNumber) <> "] Error - " <> (errorText error.typ error.offender)
    suffix EOF = ", at end"
    suffix t   = ", at \"" <> (showText t) <> "\""
    errorText InvalidExpression   token = "Expected an expression" <> (suffix token)
    errorText MissingClosingParen token = "Expected ')' after expression" <> (suffix token)

scannerErrorAsText :: ScannerError -> Text
scannerErrorAsText error = line
  where
    line = "[line " <> (showText error.lineNumber) <> "] Error - " <> (errorText error.typ)
    errorText (InvalidNumberFormat c) = "Invalid number format: " <> c
    errorText (UnknownToken c)        = "Unknown token: " <> c
    errorText UnterminatedString      = "Unterminated string"
