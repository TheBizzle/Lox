module Main(main) where

import Control.DeepSeq(force)
import Control.Monad.State(runStateT)

import GHC.Real(realToFrac)

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(ExitFailure))

import Lox.Evaluator.EvalError(
    EvalError(ArityMismatch, CanOnlyRefSuperInsideClass, CanOnlyRefThisInsideClass, ClassesCanOnlyContainFns, ClassNotFound, NotAClass, NotAnObject, NotCallable, NotImplemented, ObjectLacksKey, SuperCannotBeSelf, SuperMustBeAClass, ThisClassHasNoSupers, TopLevelReturn, TypeError, UnknownVariable)
  )

import Lox.Evaluator.Program(definePrimitiveFunc, Program, ProgramState)
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
import qualified Lox.Evaluator.Program     as Program


main :: IO ()
main = getArgs >>= processArgs
  where
    processArgs :: [String] -> IO ()
    processArgs            [] = Exception.handle handler $ initialized >>= runPrompt
    processArgs (filePath:[]) = (TIO.readFile filePath) >>= runFile
    processArgs             _ = (TIO.putStrLn "Usage: lox [script]") >> (exitWith $ ExitFailure 64)

    handler :: Exception.IOException -> IO ()
    handler _ = TIO.putStrLn "EOF reached.  Exiting...." >> return ()

runPrompt :: ProgramState -> IO ()
runPrompt program =
  do
    putStrFlush "> "
    line <- TIO.getLine
    when (line /= "exit") $ (run program line) >>= (fst &> runPrompt)

initialized :: IO ProgramState
initialized = (runStateT (forM_ primitives $ uncurry3 definePrimitiveFunc) Program.empty) <&> snd
  where
    primitives = [clock]

    clock        = ("clock", [], clocker)
    clocker _ [] = (liftIO Clock.getPOSIXTime) <&> (realToFrac &> NumberV &> CF.Normal &> Success)
    clocker _  _ = error "Invalid number of arguments to `clock`; expects zero."

runFile :: Text -> IO ()
runFile code = initialized >>= (flip run code) >=> \(_, errorCode) -> when (errorCode /= 0) $ exitWith $ ExitFailure errorCode

run :: ProgramState -> Text -> IO (ProgramState, Int)
run state code =
  case (interpret code) of
    ScannerFailure errors  -> (handleError scannerErrorAsText errors) $> (state, 65)
    ParserFailure  errors  -> (handleError  parserErrorAsText errors) $> (state, 65)
    OtherFailure   errors  -> (handleError          anyAsText errors) $> (state, 65)
    Succeeded      program -> runProgram program state

handleError :: Traversable t => (a -> Text) -> t a -> IO ()
handleError errorToText errors = for_ errors $ errorToText &> TIO.putStrLn

runProgram :: Program (Validation (NonEmpty EvalError) Value) -> ProgramState -> IO (ProgramState, Int)
runProgram program state =
  do
    (resultV, newState) <- runStateT program state
    output              <- validation handleBad handleGood resultV
    let !strictState     = force newState
    return (strictState, output)
  where
    handleBad  = (handleError evalErrorAsText) &> ($> 70)
    handleGood = showText &> TIO.putStrLn      &> ($>  0)

anyAsText :: a -> Text
anyAsText _ = error "Unimplemented error handler"

evalErrorAsText :: EvalError -> Text
evalErrorAsText = errorText
  where
    lineNum tp = showText tp.lineNumber

    errorText (ArityMismatch    tp wd gt)     = "Runtime error (on line " <> (lineNum tp) <> "): Expected " <> (showText wd) <> " arguments, but got " <> (showText gt)
    errorText (CanOnlyRefSuperInsideClass tp) = "Runtime error (on line " <> (lineNum tp) <> "): `super` can only can referenced inside of a class"
    errorText (CanOnlyRefThisInsideClass  tp) = "Runtime error (on line " <> (lineNum tp) <> "): `this` can only can referenced inside of a class"
    errorText (ClassNotFound    name)         = "Runtime error (on line ???): Did not find any value matching class name \"" <> name <> "\""
    errorText (NotAClass        value)        = "Runtime error (on line ???): This value is not a class: " <> (showText value)
    errorText (NotAnObject      value)        = "Runtime error (on line ???): This value is not an object: " <> (showText value)
    errorText (NotCallable      tp)           = "Runtime error (on line " <> (lineNum tp) <> "): Only functions and classes are callable"
    errorText (NotImplemented   tp)           = "Runtime error (on line " <> (lineNum tp) <> "): Functionality not yet implemented"
    errorText (ObjectLacksKey   name)         = "Runtime error (on line ???): This object does not have anything named \"" <> name <> "\""
    errorText (ClassesCanOnlyContainFns tp)   = "Runtime error (on line " <> (lineNum tp) <> "): Class bodies may only contain function definitions"
    errorText (UnknownVariable   tp name)     = "Runtime error (on line " <> (lineNum tp) <> "): `" <> name <> "` is not defined"
    errorText (SuperCannotBeSelf tp name)     = "Runtime error (on line " <> (lineNum tp) <> "): `" <> name <> "` can't inherit from itself"
    errorText (SuperMustBeAClass tp name)     = "Runtime error (on line " <> (lineNum tp) <> "): Superclass `" <> name <> "` must be a class"
    errorText TopLevelReturn                  = "Runtime error (on line ???): `return` is only allowed inside functions"
    errorText (ThisClassHasNoSupers tp)       = "Runtime error (on line " <> (lineNum tp) <> "): Can't use `super` in a class with no superclass"
    errorText (TypeError t pairs)             = Text.intercalate "\n" $ map (typeError t) pairs
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
