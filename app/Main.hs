module Main(main) where

import Control.DeepSeq(force)
import Control.Monad.State(runStateT)

import GHC.Real(realToFrac)

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(ExitFailure))
import System.IO(stderr)

import Lox.Evaluator.EvalError(
    EvalError(ArityMismatch, CanOnlyGetObj, CanOnlyRefSuperInsideClass, CanOnlyRefThisInsideClass, CanOnlySetObj, ClassesCanOnlyContainFns, ClassNotFound, NotAClass, NotCallable, NotImplemented, ObjectLacksKey, OperandMustBeNumber, OperandsMustBeNumbers, OperandsMustBeNumsOrStrs, SuperCannotBeSelf, SuperMustBeAClass, ThisClassHasNoSupers, TopLevelReturn, UnknownVariable)
  )

import Lox.Evaluator.Program(definePrimitiveFunc, Program, ProgramState)
import Lox.Evaluator.Value(Value(Nada, NumberV))

import Lox.Interpreter(interpret, Result(OtherFailure, ParserFailure, ScannerFailure, Succeeded))

import Lox.Parser.ParserError(
    ParserError(lineNumber, offender, typ),
    ParserErrorType(Backtrack, ExpectedIdentifier, InvalidExpression, Missing, ReservedName, TooMuchArguing)
  )

import Lox.Scanner.ScannerError(
    ScannerError(lineNumber, typ),
    ScannerErrorType(InvalidNumberFormat, UnknownToken, UnterminatedString)
  )

import Lox.Scanner.Token(Token(EOF, LeftBrace, LeftParen), TokenPlus(lineNumber))

import qualified Control.Exception     as Exception
import qualified Data.Time.Clock.POSIX as Clock
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
    when (line /= "exit") $ do
      (state, _) <- run program line
      runPrompt state

initialized :: IO ProgramState
initialized = (runStateT (forM_ primitives $ uncurry3 definePrimitiveFunc) Program.empty) <&> snd
  where
    primitives = [clock]

    clock        = ("clock", [], clocker)
    clocker _ [] = (liftIO Clock.getPOSIXTime) <&> (realToFrac &> NumberV &> CF.Normal &> Success)
    clocker _  _ = error "Invalid number of arguments to `clock`; expects zero."

runFile :: Text -> IO ()
runFile code =
  do
    initted        <- initialized
    (_, errorCode) <- run initted code
    when (errorCode /= 0) $
      exitWith $ ExitFailure errorCode

run :: ProgramState -> Text -> IO (ProgramState, Int)
run state code =
  case (interpret code) of
    ScannerFailure errors  -> (handleError scannerErrorAsText errors) $> (state, 65)
    ParserFailure  errors  -> (handleError  parserErrorAsText errors) $> (state, 65)
    OtherFailure   errors  -> (handleError          anyAsText errors) $> (state, 65)
    Succeeded      program -> runProgram program state

handleError :: Traversable t => (a -> Text) -> t a -> IO ()
handleError errorToText errors = for_ errors $ errorToText &> TIO.hPutStrLn stderr

runProgram :: Program (Validation (NonEmpty EvalError) Value) -> ProgramState -> IO (ProgramState, Int)
runProgram program state =
  do
    (resultV, newState) <- runStateT program state
    output              <- validation handleBad handleGood resultV
    let !strictState     = force newState
    return (strictState, output)
  where
    handleBad = (handleError evalErrorAsText) &> ($> 70)

    handleGood Nada   = return 0
    handleGood result = result |> showText &> TIO.putStrLn &> ($> 0)

anyAsText :: a -> Text
anyAsText _ = error "Unimplemented error handler"

evalErrorAsText :: EvalError -> Text
evalErrorAsText = errorText
  where
    suffix token s = s <> "\n[line " <> (showText token.lineNumber) <> "]"

    errorText (ArityMismatch    tp wd gt)     = suffix tp $ "Expected " <> (showText wd) <> " arguments but got " <> (showText gt) <> "."
    errorText (CanOnlyGetObj tp)              = suffix tp $ "Only instances have properties."
    errorText (CanOnlyRefSuperInsideClass tp) = suffix tp $ "Error at 'super': Can't use 'super' outside of a class."
    errorText (CanOnlyRefThisInsideClass  tp) = suffix tp $ "Error at 'this': Can't use 'this' outside of a class."
    errorText (CanOnlySetObj tp)              = suffix tp $ "Only instances have fields."
    errorText (ClassNotFound    tp name)      = suffix tp $ "Did not find any value matching class name \"" <> name <> "\""
    errorText (NotAClass        tp value)     = suffix tp $ "This value is not a class: " <> (showText value)
    errorText (NotCallable      tp)           = suffix tp $ "Can only call functions and classes."
    errorText (NotImplemented   tp)           = suffix tp $ "Functionality not yet implemented"
    errorText (ObjectLacksKey   tp name)      = suffix tp $ "Undefined property \'" <> name <> "\'."
    errorText (OperandMustBeNumber tp)        = suffix tp $ "Operand must be a number."
    errorText (OperandsMustBeNumbers tp)      = suffix tp $ "Operands must be numbers."
    errorText (OperandsMustBeNumsOrStrs tp)   = suffix tp $ "Operands must be two numbers or two strings."
    errorText (ClassesCanOnlyContainFns tp)   = suffix tp $ "Class bodies may only contain function definitions"
    errorText (UnknownVariable   tp name)     = suffix tp $ "Undefined variable '" <> name <> "'."
    errorText (SuperCannotBeSelf tp name)     = suffix tp $ "`" <> name <> "` can't inherit from itself"
    errorText (SuperMustBeAClass tp    _)     = suffix tp $ "Superclass must be a class."
    errorText (TopLevelReturn tp)             = suffix tp $ "`return` is only allowed inside functions"
    errorText (ThisClassHasNoSupers tp)       = suffix tp $ "Can't use `super` in a class with no superclass"

parserErrorAsText :: ParserError -> Text
parserErrorAsText error = line
  where
    line = "[" <> (showText error.lineNumber) <> "] Error at " <> (desc error.offender) <> (errorText error.typ)
    desc EOF = "end: "
    desc t   = "'" <> (showText t) <> "': "
    errorText Backtrack           = "Expected something here (this shouldn't be able to happen)"
    errorText (ReservedName _)    = "Expect variable name."
    errorText TooMuchArguing      = "Functions are limited to 254 arguments."
    errorText ExpectedIdentifier  = "Expected an identifier"
    errorText InvalidExpression   = "Expect expression"
    errorText (Missing LeftParen) = "No matching '('"
    errorText (Missing LeftBrace) = "No matching '{'"
    errorText (Missing t)         = "Expected '" <> (showText t) <> "'"

scannerErrorAsText :: ScannerError -> Text
scannerErrorAsText error = line
  where
    line = "[" <> (showText error.lineNumber) <> "] Error: " <> (errorText error.typ)
    errorText (InvalidNumberFormat c) = "Invalid number format: " <> c
    errorText (UnknownToken _)        = "Unexpected character."
    errorText UnterminatedString      = "Unterminated string."
