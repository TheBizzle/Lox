module Lox.Evaluator.Internal.Type(Type(AnyT, BooleanT, NumberT, StringT), typecheck) where

import Lox.Scanner.Token(Token(Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Minus, Plus, Slash, Star))

import Lox.Evaluator.Internal.Value(Value(BooleanV, NumberV, StringV))


data Type
  =     AnyT
  | BooleanT
  |  NumberT
  |  StringT

instance Show Type where
  show     AnyT = "any"
  show BooleanT = "boolean"
  show  NumberT = "number"
  show  StringT = "string"

typecheck :: Token -> [Value] -> [Maybe (Type, Value)]
typecheck Bang         [v]    = [check    AnyT v]
typecheck BangEqual    [v, w] = [check    AnyT v, check    AnyT w]
typecheck EqualEqual   [v, w] = [check    AnyT v, check    AnyT w]
typecheck Greater      [v, w] = [check NumberT v, check NumberT w]
typecheck GreaterEqual [v, w] = [check NumberT v, check NumberT w]
typecheck Less         [v, w] = [check NumberT v, check NumberT w]
typecheck LessEqual    [v, w] = [check NumberT v, check NumberT w]
typecheck Minus        [v]    = [check NumberT v]
typecheck Minus        [v, w] = [check NumberT v, check NumberT w]
typecheck Plus         [v, w] = [check StringT v, check StringT w]
typecheck Slash        [v, w] = [check NumberT v, check NumberT w]
typecheck Star         [v, w] = [check NumberT v, check NumberT w]

check :: Type -> Value -> Maybe (Type, Value)
check     AnyT            _ = Nothing
check BooleanT (BooleanV _) = Nothing
check  NumberT ( NumberV _) = Nothing
check  StringT ( StringV _) = Nothing
check        t            v = Just (t, v)
