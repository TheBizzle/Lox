module Lox.Evaluator.Internal.Data(Environment, ScopeAddress(n, ScopeAddress), VarAddress(scopeAddr, VarAddress, varName)) where


newtype ScopeAddress
  = ScopeAddress { n :: Word }
  deriving (Eq, Ord, Show)

data VarAddress
  = VarAddress { varName :: Text, scopeAddr :: ScopeAddress }
  deriving (Eq, Ord, Show)
