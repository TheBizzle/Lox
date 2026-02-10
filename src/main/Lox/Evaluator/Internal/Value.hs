module Lox.Evaluator.Internal.Value(Class(baseEnv, Class, cName, initFnM, methodFns, superclassM), Function(argNames, env, Function, idNum, name, owner), Object(instanceID, myClass, Object), Value(BooleanV, ClassV, clazz, function, FunctionV, isNative, Nada, NilV, NumberV, object, ObjectV, StringV)) where

import Control.DeepSeq(NFData, rnf)

import Text.Printf(printf)

import Lox.Evaluator.Internal.Data(Environment, ScopeAddress)

import qualified Lox.Parser.AST as AST
import qualified Data.Text      as Text


data Class
  = Class { cName :: Text, superclassM :: Maybe Class, initFnM :: Maybe AST.Function, methodFns :: [AST.Function], baseEnv :: Environment }
  deriving (Eq, Show)

data Function
  = Function { name :: Text, argNames :: [Text], env :: Environment, idNum :: Word, owner :: ScopeAddress }
  deriving (Eq, Show)

data Object
  = Object { myClass :: Class, instanceID :: Word }
  deriving (Eq, Show)

instance NFData Class where
  -- Hopefully, I don't need to force the method outlines to evaluate early --Jason B. (1/6/26)
  rnf (Class cn scm _ _ env) = rnf cn `seq` rnf scm `seq` rnf env

instance NFData Function where
  rnf (Function n as env id o) = rnf n `seq` rnf as `seq` rnf env `seq` rnf id `seq` rnf o

instance NFData Object where
  rnf (Object c id) = rnf c `seq` rnf id

data Value
  = BooleanV  Bool
  | ClassV    { clazz :: Class }
  | FunctionV { isNative :: Bool, function :: Function }
  | NumberV   Double
  | ObjectV   { object :: Object }
  | StringV   Text
  | Nada
  | NilV
  deriving Eq

instance NFData Value where
  rnf (BooleanV  x  ) = rnf x
  rnf (ClassV    x  ) = rnf x
  rnf (FunctionV x y) = rnf x `seq` rnf y
  rnf (NumberV   x  ) = rnf x
  rnf (ObjectV   x  ) = rnf x
  rnf (StringV   x  ) = rnf x
  rnf  Nada         = ()
  rnf  NilV         = ()

instance Show Value where
  show (BooleanV x)                               = x |> showText &> Text.toLower &> asString
  show (ClassV (Class cName _ _ _ _))             = asString cName
  show (FunctionV True                        _)  = "<native fn>"
  show (FunctionV False (Function name _ _ _ _))  = "<fn " <> (asString name) <> ">"
  show Nada                                       = "you_cant_see_this"
  show NilV                                       = "nil"
  show (ObjectV (Object (Class cName _ _ _ _) _)) = (asString cName) <> " instance"
  show (NumberV x)                                = showNum x
  show (StringV x)                                = asString x

showNum :: Double -> String
showNum = asFP &> asText &> removeTrailingZeroes &> asString
  where
    asFP = printf "%.9f"

    removeTrailingZeroes = Text.dropWhileEnd (== '0') &> (id &&& (Text.stripSuffix ".")) &> (\(a, b) -> maybe a id b)
