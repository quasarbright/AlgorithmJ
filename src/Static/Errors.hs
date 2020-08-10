module Static.Errors where

import Syntax.Names
import Syntax.Types
import Parsing.ParseUtils(SS)
import qualified Parsing.ParseAst as P

data StaticError = Mismatch MonoType MonoType
               | OccursError TVName MonoType
               | UnboundVar VName
               | UnboundVarOp VarOpName
               | UnboundCon CName
               | UnboundConOp ConOpName
               | EmptyCase
               | InvalidExpr (P.Expr SS) SS
               | InvalidPattern (P.Expr SS) SS
               | InvalidType (P.Type SS) SS
               | InvalidDecl (P.Decl SS) SS
               deriving(Eq, Ord, Show)