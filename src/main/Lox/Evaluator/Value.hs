{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Lox.Evaluator.Value(module Lox.Evaluator.Internal.Program.Value) where

import Lox.Evaluator.Internal.Program.Value(
    Value(BooleanV, ClassV, clazz, function, FunctionV, isNative, Nada, NilV, NumberV, object, ObjectV, StringV)
  )
