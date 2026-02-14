module Main(main) where

import Control.DeepSeq(force)
import Control.Monad.State(execStateT, runStateT)

import GHC.Real(realToFrac)

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(ExitFailure))
import System.IO(stderr)

import Lox.Evaluator.EvalError(
    EvalError(EvalError)
  , EvalErrorType(ArityMismatch, CanOnlyGetObj, CanOnlyRefSuperInsideClass, CanOnlyRefThisInsideClass, CanOnlySetObj, ClassNotFound, NotAClass, NotCallable, NotImplemented, ObjectLacksKey, OperandMustBeNumber, OperandsMustBeNumbers, OperandsMustBeNumsOrStrs, SuperCannotBeSelf, SuperMustBeAClass, ThisClassHasNoSupers, TopLevelReturn, UnknownVariable)
  )

import Lox.Evaluator.Program(definePrimitiveFunc, Program, ProgramState)
import Lox.Evaluator.Value(Value(Nada, NumberV))

import Lox.Interpreter(interpret, Result(OtherFailure, ParserFailure, ScannerFailure, Succeeded, VerifierFailure))

import Lox.Parser.ParserError(
    ParserError(offender, typ),
    ParserErrorType(Backtrack, ExpectedIdentifier, InvalidExpression, Missing, ReservedName, TooMuchArguing)
  )

import Lox.Scanner.ScannerError(
    ScannerError(lineNumber, typ),
    ScannerErrorType(InvalidNumberFormat, UnknownToken, UnterminatedString)
  )

import Lox.Scanner.Token(SourceLoc(lineNumber), Token(EOF, LeftBrace, LeftParen), TokenPlus(loc, token))

import Lox.Verify.VerifierError(VerifierError(offender, typ), VerifierErrorType(DuplicateVar))

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
      (state, _) <- run True program line
      runPrompt state

initialized :: IO ProgramState
initialized = (execStateT (forM_ primitives $ uncurry3 definePrimitiveFunc) Program.empty)
  where
    primitives = [clock]

    clock        = ("clock", [], clocker)
    clocker _ [] = (liftIO Clock.getPOSIXTime) <&> (realToFrac &> NumberV &> CF.Normal &> Success)
    clocker _  _ = error "Invalid number of arguments to `clock`; expects zero."

runFile :: Text -> IO ()
runFile code =
  do
    initted        <- initialized
    (_, errorCode) <- run False initted code
    when (errorCode /= 0) $
      exitWith $ ExitFailure errorCode

run :: Bool -> ProgramState -> Text -> IO (ProgramState, Int)
run isREPL state code =
  case (interpret code) of
    ScannerFailure  errors  -> (handleError  scannerErrorAsText errors) $> (state, 65)
    ParserFailure   errors  -> (handleError   parserErrorAsText errors) $> (state, 65)
    VerifierFailure errors  -> (handleError verifierErrorAsText errors) $> (state, 65)
    OtherFailure    errors  -> (handleError           anyAsText errors) $> (state, 65)
    Succeeded       program -> runProgram isREPL program state

handleError :: Traversable t => (a -> Text) -> t a -> IO ()
handleError errorToText errors = for_ errors $ errorToText &> TIO.hPutStrLn stderr

runProgram :: Bool -> Program (Validation (NonEmpty EvalError) Value) -> ProgramState -> IO (ProgramState, Int)
runProgram isREPL program state =
  do
    (resultV, newState) <- runStateT program state
    output              <- validation handleBad handleGood resultV
    let !strictState     = force newState
    return (strictState, output)
  where
    handleBad = (handleError evalErrorAsText) &> ($> 70)

    handleGood Nada   = return 0
    handleGood result =
      if isREPL then
        result |> showText &> TIO.putStrLn &> ($> 0)
      else
        return 0

anyAsText :: a -> Text
anyAsText _ = error "Unimplemented error handler"

evalErrorAsText :: EvalError -> Text
evalErrorAsText (EvalError typ tp) = suffix tp $ errorText typ
  where
    suffix token s = s <> "\n[line " <> (showText token.loc.lineNumber) <> "]"

    errorText (ArityMismatch wd gt)      = "Expected " <> (showText wd) <> " arguments but got " <> (showText gt) <> "."
    errorText CanOnlyGetObj              = "Only instances have properties."
    errorText CanOnlyRefSuperInsideClass = "Error at 'super': Can't use 'super' outside of a class."
    errorText CanOnlyRefThisInsideClass  = "Error at 'this': Can't use 'this' outside of a class."
    errorText CanOnlySetObj              = "Only instances have fields."
    errorText (ClassNotFound name)       = "Did not find any value matching class name \"" <> name <> "\""
    errorText (NotAClass value)          = "This value is not a class: " <> (showText value)
    errorText NotCallable                = "Can only call functions and classes."
    errorText NotImplemented             = "Functionality not yet implemented"
    errorText (ObjectLacksKey name)      = "Undefined property \'" <> name <> "\'."
    errorText OperandMustBeNumber        = "Operand must be a number."
    errorText OperandsMustBeNumbers      = "Operands must be numbers."
    errorText OperandsMustBeNumsOrStrs   = "Operands must be two numbers or two strings."
    errorText (UnknownVariable   name)   = "Undefined variable '" <> name <> "'."
    errorText (SuperCannotBeSelf name)   = "`" <> name <> "` can't inherit from itself"
    errorText (SuperMustBeAClass _)      = "Superclass must be a class."
    errorText TopLevelReturn             = "`return` is only allowed inside functions"
    errorText ThisClassHasNoSupers       = "Can't use `super` in a class with no superclass"

parserErrorAsText :: ParserError -> Text
parserErrorAsText error = line
  where
    line = "[" <> (showText error.offender.loc.lineNumber) <> "] Error at " <> (desc error.offender.token) <> (errorText error.typ)
    desc EOF = "end: "
    desc t   = "'" <> (showText t) <> "': "
    errorText Backtrack           = "Expected something here (this shouldn't be able to happen)"
    errorText ReservedName        = "Expect variable name."
    errorText TooMuchArguing      = "Functions are limited to 254 arguments."
    errorText ExpectedIdentifier  = "Expected an identifier"
    errorText InvalidExpression   = "Expect expression"
    errorText (Missing LeftParen) = "No matching '('"
    errorText (Missing LeftBrace) = "No matching '{'"
    errorText (Missing t)         = "Expected '" <> (showText t) <> "'"

scannerErrorAsText :: ScannerError -> Text
scannerErrorAsText error = line
  where
    line = "[line " <> (showText error.lineNumber) <> "] Error: " <> (errorText error.typ)
    errorText (InvalidNumberFormat c) = "Invalid number format: " <> c
    errorText (UnknownToken _)        = "Unexpected character."
    errorText UnterminatedString      = "Unterminated string."

verifierErrorAsText :: VerifierError -> Text
verifierErrorAsText error = line
  where
    line = "[line " <> (showText error.offender.loc.lineNumber) <> "] Error at " <> (desc error.offender.token) <> (errorText error.typ)
    desc EOF = "end: "
    desc t   = "'" <> (showText t) <> "': "
    errorText DuplicateVar = "Already a variable with this name in this scope."
