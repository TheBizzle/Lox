module Lox.Evaluator.Internal.Eval.Evaluator(eval) where

import Control.Monad.State(modify)

import Lox.Parser.AST(AST(statements), Function(fnDecl), Statement(FunctionStatement), Variable(varName))

import Lox.Evaluator.Internal.Program.EvalError(
    EvalError(EvalError)
  , EvalErrorType(TopLevelReturn)
  )

import Lox.Evaluator.Internal.Program.Program(Evaluating, Evaluated)
import Lox.Evaluator.Internal.Program.Variable(declareVar)
import Lox.Evaluator.Internal.Program.Value(Value(Nada))

import Lox.Evaluator.Internal.Eval.Statement(runStatements)

import qualified Data.List.NonEmpty as NE

import qualified Lox.Evaluator.Internal.Program.ControlFlow as CF


eval :: AST -> Evaluated
eval = statements &> runTopLevelStatements &> evaluated

evaluated :: Evaluating -> Evaluated
evaluated = (<&> (validation Failure helper))
  where
    helper (CF.Exception    ex   ) = Failure $ NE.singleton ex
    helper (CF.Normal    value   ) = Success value
    helper (CF.Return    token  _) = Failure $ NE.singleton $ EvalError TopLevelReturn token

runTopLevelStatements :: [Statement] -> Evaluating
runTopLevelStatements statements =
  do
    mapM_ hoistIfFn statements
    runStatements statements
  where
    hoistIfFn (FunctionStatement fn) = modify $ declareVar fn.fnDecl.varName Nada
    hoistIfFn                      _ = return ()
