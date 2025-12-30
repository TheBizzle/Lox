module Lox.Evaluator.Internal.Data(Environment, ScopeAddress(n, ScopeAddress), VarAddress(scopeAddr, VarAddress, varName)) where

import Control.DeepSeq(NFData, rnf)


type Environment = Map Text VarAddress

newtype ScopeAddress
  = ScopeAddress { n :: Word }
  deriving (Eq, Ord, Show)

instance NFData ScopeAddress where
  rnf (ScopeAddress n) = rnf n

data VarAddress
  = VarAddress { varName :: Text, scopeAddr :: ScopeAddress }
  deriving (Eq, Ord, Show)

instance NFData VarAddress where
  rnf (VarAddress vn addr) = rnf vn `seq` rnf addr
