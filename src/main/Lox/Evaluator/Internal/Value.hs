module Lox.Evaluator.Internal.Value(Value(argNames, BooleanV, env, FunctionV, idNum, name, NilV, NumberV, owner, StringV)) where

import Control.DeepSeq(NFData, rnf)

import Text.Printf(printf)

import Lox.Evaluator.Internal.Data(Environment, ScopeAddress)

import qualified Data.Text as Text


data Value
  = BooleanV Bool
  | FunctionV { name :: Text, argNames :: [Text], env :: Environment, idNum :: Word, owner :: ScopeAddress }
  | NumberV  Double
  | StringV  Text
  | NilV
  deriving Eq

instance NFData Value where
  rnf (BooleanV x)              = rnf x
  rnf (FunctionV n as env id o) = rnf n `seq` rnf as `seq` rnf env `seq` rnf id `seq` rnf o
  rnf (NumberV x)               = rnf x
  rnf (StringV x)               = rnf x
  rnf  NilV                     = ()

instance Show Value where
  show (BooleanV x)                = x |> showText &> Text.toLower &> asString
  show (FunctionV name args _ _ _) = "<function " <> (asString name) <> "(" <> (asString $ Text.intercalate "," args) <> "){ ... }>"
  show NilV                        = "nil"
  show (NumberV x)                 = showNum x
  show (StringV x)                 = "\"" <> asString x <> "\""

showNum :: Double -> String
showNum = asFP &> asText &> removeTrailingZeroes &> asString
  where
    asFP = printf "%.9f"

    removeTrailingZeroes = (id &&& stripSuffix) &> (\(a, b) -> maybe a id b)

    stripSuffix = Text.dropWhileEnd (== '0') &> Text.stripSuffix "."
