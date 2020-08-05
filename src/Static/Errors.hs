module Static.Errors where

import Syntax.Names
import Syntax.Types

data TypeError = Mismatch MonoType MonoType
               | OccursError TVName MonoType
               | UnboundVar VName
               | UnboundCon CName
               | EmptyCase
               deriving(Eq, Ord, Show)