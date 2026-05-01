module Lox.Evaluator.Internal.Eval.Common(asBool, asObject, fail, lose, nothing, onSuccessEval, onSuccessEval2, onSuccessEval2Seq, succeed, win) where

import Lox.Evaluator.Internal.ControlFlow(ControlFlow(Normal))
import Lox.Evaluator.Internal.EvalError(EvalError)
import Lox.Evaluator.Internal.Program(Evaluating, Prog)
import Lox.Evaluator.Internal.Value(Value(BooleanV, Nada, NilV, ObjectV))

import qualified Data.List          as List
import qualified Data.List.NonEmpty as NE

import qualified Lox.Evaluator.Internal.Value as V


type Result t  = Validation (NonEmpty EvalError) t

asBool :: Value -> Bool
asBool NilV             = False
asBool (BooleanV False) = False
asBool _                = True

asObject :: Value -> EvalError -> Result V.Object
asObject (ObjectV obj) _ = Success obj
asObject             _ e = Failure $ NE.singleton e

onSuccessEval :: Result ControlFlow -> (Value -> Evaluating) -> Evaluating
onSuccessEval vali f = vali `failOrM` runIfNormal
  where
    runIfNormal (Normal v) = f v
    runIfNormal          x = return $ Success x

onSuccessEval2 :: Result (ControlFlow, ControlFlow) -> ((Value, Value) -> Evaluating) -> Evaluating
onSuccessEval2 vali f = vali `failOrM` runIfNormal
  where
    runIfNormal ((Normal v), (Normal w)) = f (v, w)
    runIfNormal ((Normal _),          x) = return $ Success x
    runIfNormal (        x,           _) = return $ Success x

onSuccessEval2Seq :: Result (ControlFlow, [ControlFlow]) -> ((Value, [Value]) -> Evaluating) -> Evaluating
onSuccessEval2Seq vali f = vali `failOrM` runIfNormal
  where
    runIfNormal ((Normal v), ws) = either (Success &> return) ((v, ) &> f) $ purify [] ws
    runIfNormal (         x,  _) = return $ Success x

    purify acc             [] = Right $ List.reverse acc
    purify acc ((Normal v):t) = purify (v:acc) t
    purify   _ (         h:_) = Left h

nothing :: Evaluating
nothing = win Nada

fail :: EvalError -> Result a
fail err = Failure $ NE.singleton err

succeed :: Value -> Result ControlFlow
succeed = Normal &> Success

win :: Value -> Evaluating
win = succeed &> return

lose :: EvalError -> Prog a
lose = fail &> return
