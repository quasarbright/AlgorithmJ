module Static.Errors where

import Syntax.Names
import Syntax.Types

data StaticError = Mismatch MonoType MonoType
               | OccursError TVName MonoType
               | UnboundVar VName
               | UnboundVarOp VarOpName
               | UnboundCon CName
               | UnboundConOp ConOpName
               | EmptyCase
               deriving(Eq, Ord, Show)