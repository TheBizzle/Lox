module Main(main) where

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(ExitFailure))

import Lox.Evaluator.Effect(Effect(Print))
import Lox.Evaluator.EvalError(EvalError(NotImplemented, TypeError, UnknownVariable))
import Lox.Evaluator.World(World, WorldState(effects))
import Lox.Evaluator.Value(Value)

import Lox.Interpreter(interpret, Result(OtherFailure, ParserFailure, ScannerFailure, Succeeded))

import Lox.Parser.ParserError(
    ParserError(lineNumber, offender, typ),
    ParserErrorType(Backtrack, InvalidExpression, InvalidStatement, MissingClosingParen, MissingSemicolon)
  )

import Lox.Scanner.ScannerError(ScannerError(lineNumber, typ), ScannerErrorType(InvalidNumberFormat, UnknownToken, UnterminatedString))
import Lox.Scanner.Token(Token(EOF), TokenPlus(lineNumber, token))

import qualified Data.List    as List
import qualified Data.Text    as Text
import qualified Data.Text.IO as TIO

import qualified Lox.Evaluator.World as World


main :: IO ()
main = getArgs >>= processArgs
  where
    processArgs :: [String] -> IO ()
    processArgs            [] = runPrompt World.empty
    processArgs (filePath:[]) = (TIO.readFile filePath) >>= runFile
    processArgs             _ = (TIO.putStrLn "Usage: lox [script]") >> (exitWith $ ExitFailure 64)

runPrompt :: WorldState -> IO ()
runPrompt world =
  do
    putStrFlush "> "
    line <- TIO.getLine
    when (line /= "exit") $ (run world line) >>= (fst &> runPrompt)

runFile :: Text -> IO ()
runFile = (run World.empty) >=> \(_, errorCode) -> when (errorCode /= 0) $ exitWith $ ExitFailure errorCode

run :: WorldState -> Text -> IO (WorldState, Int)
run world code =
  case (interpret code) of
    ScannerFailure errors  -> (handleError scannerErrorAsText errors) $> (world, 65)
    ParserFailure  errors  -> (handleError  parserErrorAsText errors) $> (world, 65)
    OtherFailure   errors  -> (handleError          anyAsText errors) $> (world, 65)
    Succeeded      program -> runWorld program world

handleError :: Traversable t => (a -> Text) -> t a -> IO ()
handleError errorToText errors = for_ errors $ errorToText &> TIO.putStrLn

runWorld :: World (Validation (NonEmpty EvalError) Value) -> WorldState -> IO (WorldState, Int)
runWorld program world = output <&> \signal -> (newWorld { effects = [] }, signal)
  where
    (resultV, newWorld) = runState program world
    output              = validation handleBad handleGood resultV
    handleBad           = (handleError evalErrorAsText) &> ($> 70)
    handleGood t        =
      do
        for_ (List.reverse newWorld.effects) runEffect
        t |> showText &> TIO.putStrLn &> ($> 0)

    runEffect (Print text) = TIO.putStrLn text

anyAsText :: a -> Text
anyAsText _ = error "Unimplemented error handler"

evalErrorAsText :: EvalError -> Text
evalErrorAsText = errorText
  where
    lineNum tp = showText tp.lineNumber
    errorText (NotImplemented tp)       = "Runtime error (on line " <> (lineNum tp) <> "): Functionality not yet implemented"
    errorText (UnknownVariable tp name) = "Runtime error (on line " <> (lineNum tp) <> "): `" <> name <> "` is not defined"
    errorText (TypeError t pairs)       = Text.intercalate "\n" $ map (typeError t) pairs
      where
        typeError tp (typ, value) = "Type error (on line " <> (lineNum tp) <> "): `" <> (showText tp.token) <>
          "` expected a value of type '" <> (showText typ) <> "', but got `" <> (showText value) <> "`"

parserErrorAsText :: ParserError -> Text
parserErrorAsText error = line
  where
    line = "[line " <> (showText error.lineNumber) <> "] Error - " <> (errorText error.typ error.offender)
    suffix EOF = ", at end"
    suffix t   = ", at \"" <> (showText t) <> "\""
    errorText Backtrack           token = withLoc token "Expected something here (this shouldn't be able to happen)"
    errorText InvalidExpression   token = withLoc token "Expected an expression"
    errorText InvalidStatement    token = withLoc token "Expected a statement"
    errorText MissingClosingParen token = withLoc token "Expected ')' after expression"
    errorText MissingSemicolon    token = withLoc token "Expected ';'"
    withLoc token = (<> (suffix token))

scannerErrorAsText :: ScannerError -> Text
scannerErrorAsText error = line
  where
    line = "[line " <> (showText error.lineNumber) <> "] Error - " <> (errorText error.typ)
    errorText (InvalidNumberFormat c) = "Invalid number format: " <> c
    errorText (UnknownToken c)        = "Unknown token: " <> c
    errorText UnterminatedString      = "Unterminated string"
