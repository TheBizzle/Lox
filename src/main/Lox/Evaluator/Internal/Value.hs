module Lox.Evaluator.Internal.Value(Value(BooleanV, NilV, NumberV, StringV)) where

import Text.Printf(printf)

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
  show (NumberV x)  = showNum x
  show (StringV x)  = "\"" <> asString x <> "\""

showNum :: Double -> String
showNum = asFP &> asText &> removeTrailingZeroes &> asString
  where
    asFP = printf "%.9f"

    removeTrailingZeroes = (id &&& stripSuffix) &> (\(a, b) -> maybe a id b)

    stripSuffix = Text.dropWhileEnd (== '0') &> Text.stripSuffix "."
