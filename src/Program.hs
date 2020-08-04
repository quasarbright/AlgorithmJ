module Program where

import Decls
import Exprs
import Types
import Data.List

data Program = Program [Decl] Expr deriving(Eq, Ord)

instance Show Program where
    show (Program decls body) = intercalate "\n\n" ((show <$> decls) ++ [show body])

-- | standard library
prelude :: Program
prelude = Program decls unit
    where
        decls =
            [ dataDecl "Bool" [] [conDecl "True" [], conDecl "False" []]
            , dataDecl "List" [1] [conDecl "Empty" [], conDecl "Cons" [tvar 1, tcon "List" [tvar 1]]]
            , dataDecl "Maybe" [1] [conDecl "Nothing" [], conDecl "Just" [tvar 1]]
            , dataDecl "Either" [1,2] [conDecl "Left" [tvar 1], conDecl "Right" [tvar 2]]
            ]

-- | prepend the first program's decls to the second program. Ghetto importing
appendPrograms :: Program -> Program -> Program
appendPrograms (Program decls _) (Program decls' body) = Program (decls++decls') body

-- combinators for program construction

-- | program with just an expression
eProg :: Expr -> Program
eProg = Program []

-- | "ghetto import" prelude
withPrelude :: Program -> Program
withPrelude = appendPrograms prelude

exprWithPrelude :: Expr -> Program
exprWithPrelude = appendPrograms prelude . eProg