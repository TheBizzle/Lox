module Lox.Interpreter(interpret, LoxFailure(ParserFailure, ScannerFailure, VerifierFailure)) where

import Data.List.NonEmpty((<|))

import Lox.Scanner.Scanner(scan)
import Lox.Scanner.ScannerError(ScannerError)
import Lox.Scanner.Token(TokenPlus)

import Lox.Parser.LoxParser(parse)
import Lox.Parser.ParserError(ParserError)

import Lox.Verifier.Verifier(verify)
import Lox.Verifier.VerifierError(VerifierError)

import Lox.Evaluator.Evaluator(eval)
import Lox.Evaluator.EvalError(EvalError)
import Lox.Evaluator.Program(Program)
import Lox.Evaluator.Value(Value)

import qualified Data.List.NonEmpty as NE


data LoxFailure
  = ScannerFailure  (NonEmpty ScannerError)
  | ParserFailure   (NonEmpty ParserError)
  | VerifierFailure (NonEmpty VerifierError)

type LoxResult = Either (NonEmpty LoxFailure) (Program (Validation (NonEmpty EvalError) Value))

interpret :: Text -> LoxResult
interpret = runNE
  where
    runNE    = scan   &> validation ( ScannerFailure &> NE.singleton &> Left) parseNE
    verifyNE = verify &> validation (VerifierFailure &> NE.singleton &> Left) evalNE
    evalNE   = eval   &> Right

    parseNE :: ([ScannerError], [TokenPlus]) -> LoxResult
    parseNE (ses, tokens) =
      case NE.nonEmpty ses of
        Just errors -> first ((ScannerFailure errors) <|) parsed
        Nothing     -> parsed
      where
        parsed = parse tokens |> validation (ParserFailure &> NE.singleton &> Left) verifyNE
