{-# LANGUAGE OverloadedRecordDot #-}

module Main(main) where

import Data.Validation(validation)

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(ExitFailure))
import System.IO(hFlush, stdout)

import Lox.Scanner(ParserError(lineNumber, typ), ParserErrorType(UnknownToken), scan, TokenPlus(token))

import qualified Data.Text.IO as TIO

main :: IO ()
main = getArgs >>= processArgs
  where
    processArgs :: [String] -> IO ()
    processArgs            [] = runPrompt
    processArgs (filePath:[]) = (TIO.readFile filePath) >>= runFile
    processArgs             _ = (TIO.putStrLn "Usage: jlox [script]") >> (exitWith $ ExitFailure 64)

runPrompt :: IO ()
runPrompt =
  do
    TIO.putStr "> "
    hFlush stdout
    line <- TIO.getLine
    when (line /= "exit") $ (run line) >> runPrompt

runFile :: Text -> IO ()
runFile code =
  do
    wasSuccess <- run code
    when (not wasSuccess) $ exitWith $ ExitFailure 65

run :: Text -> IO Bool
run code =
  do
    let result = scan code
    validation handleError handleSuccess result
  where

    handleError errors =
      do
        for_ errors $ errorAsText &> TIO.putStrLn
        return False

    handleSuccess results =
      do
        for_ results $ token &> showText &> TIO.putStrLn
        return True

errorAsText :: ParserError -> Text
errorAsText error = line
  where
    line      = "[line " <> (showText error.lineNumber) <> "] Error - " <> errorText
    errorText = case error.typ of
                  (UnknownToken c) -> "Unknown token: " <> c
