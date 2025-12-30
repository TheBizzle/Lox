module Main(main) where

import Control.Monad.State(runStateT)

import GHC.Real(realToFrac)

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(ExitFailure))

import Lox.Evaluator.EvalError(EvalError(ArityMismatch, NotCallable, NotImplemented, TopLevelReturn, TypeError, UnknownVariable))
import Lox.Evaluator.World(definePrimitiveFunc, World, WorldState)
import Lox.Evaluator.Value(Value(NumberV))

import Lox.Interpreter(interpret, Result(OtherFailure, ParserFailure, ScannerFailure, Succeeded))

import Lox.Parser.ParserError(
    ParserError(lineNumber, offender, typ),
    ParserErrorType(Backtrack, ExpectedIdentifier, InvalidExpression, Missing, ReservedName, TooMuchArguing)
  )

import Lox.Scanner.ScannerError(
    ScannerError(lineNumber, typ),
    ScannerErrorType(InvalidNumberFormat, UnknownToken, UnterminatedString)
  )

import Lox.Scanner.Token(Token(EOF, LeftBrace, LeftParen), TokenPlus(lineNumber, token))

import qualified Control.Exception     as Exception
import qualified Data.Time.Clock.POSIX as Clock
import qualified Data.Text             as Text
import qualified Data.Text.IO          as TIO

import qualified Lox.Evaluator.ControlFlow as CF
import qualified Lox.Evaluator.World       as World


main :: IO ()
main = getArgs >>= processArgs
  where
    processArgs :: [String] -> IO ()
    processArgs            [] = Exception.handle handler $ runPrompt initialWorld
    processArgs (filePath:[]) = (TIO.readFile filePath) >>= runFile
    processArgs             _ = (TIO.putStrLn "Usage: lox [script]") >> (exitWith $ ExitFailure 64)

    handler :: Exception.IOException -> IO ()
    handler _ = TIO.putStrLn "EOF reached.  Exiting...." >> return ()

runPrompt :: WorldState -> IO ()
runPrompt world =
  do
    putStrFlush "> "
    line <- TIO.getLine
    when (line /= "exit") $ (run world line) >>= (fst &> runPrompt)

initialWorld :: WorldState
initialWorld = foldr (uncurry3 definePrimitiveFunc) World.empty primitives
  where
    primitives = [clock]

    clock        = ("clock", [], clocker)
    clocker _ [] = (liftIO Clock.getPOSIXTime) <&> (realToFrac &> NumberV &> CF.Normal &> Success)
    clocker _  _ = error "Invalid number of arguments to `clock`; expects zero."

runFile :: Text -> IO ()
runFile = (run initialWorld) >=> \(_, errorCode) -> when (errorCode /= 0) $ exitWith $ ExitFailure errorCode

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
runWorld program world =
  do
    (resultV, newWorld) <- runStateT program world
    output              <- validation handleBad handleGood resultV
    return (newWorld, output)
  where
    handleBad  = (handleError evalErrorAsText) &> ($> 70)
    handleGood = showText &> TIO.putStrLn      &> ($>  0)

anyAsText :: a -> Text
anyAsText _ = error "Unimplemented error handler"

evalErrorAsText :: EvalError -> Text
evalErrorAsText = errorText
  where
    lineNum tp = showText tp.lineNumber

    errorText (ArityMismatch  tp wd gt) = "Runtime error (on line " <> (lineNum tp) <> "): Expected " <> (showText wd) <> " arguments, but got " <> (showText gt)
    errorText (NotCallable    tp)       = "Runtime error (on line " <> (lineNum tp) <> "): Only functions and classes are callable"
    errorText (NotImplemented tp)       = "Runtime error (on line " <> (lineNum tp) <> "): Functionality not yet implemented"
    errorText (UnknownVariable tp name) = "Runtime error (on line " <> (lineNum tp) <> "): `" <> name <> "` is not defined"
    errorText TopLevelReturn            = "Runtime error (on line ???): `return` is only allowed inside functions"
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
    errorText (ReservedName _)    token = withLoc token "Illegal use of reserved name"
    errorText TooMuchArguing      token = withLoc token "Functions are limited to 254 arguments."
    errorText ExpectedIdentifier  token = withLoc token "Expected an identifier"
    errorText InvalidExpression   token = withLoc token "Expected an expression"
    errorText (Missing LeftParen) token = withLoc token $ "No matching '('"
    errorText (Missing LeftBrace) token = withLoc token $ "No matching '{'"
    errorText (Missing t)         token = withLoc token $ "Expected '" <> (showText t) <> "'"
    withLoc token = (<> (suffix token))

scannerErrorAsText :: ScannerError -> Text
scannerErrorAsText error = line
  where
    line = "[line " <> (showText error.lineNumber) <> "] Error - " <> (errorText error.typ)
    errorText (InvalidNumberFormat c) = "Invalid number format: " <> c
    errorText (UnknownToken c)        = "Unknown token: " <> c
    errorText UnterminatedString      = "Unterminated string"
