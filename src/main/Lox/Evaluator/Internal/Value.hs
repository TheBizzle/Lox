module Lox.Evaluator.Internal.Value(Value(BooleanV, NilV, NumberV, StringV)) where

import qualified Data.Text as Text


data Value
  = BooleanV Bool
  | NumberV  Double
  | StringV  Text
  | NilV
  deriving Eq

instance Show Value where
  show (BooleanV x) = x |> showText &> Text.toLower &> asString
  show NilV         = "nil"
  show (NumberV x)  = x |> showText &> ((id &&& Text.stripSuffix ".0") &> (\(a, b) -> maybe a id b)) &> asString
  show (StringV x)  = "\"" <> asString x <> "\""
