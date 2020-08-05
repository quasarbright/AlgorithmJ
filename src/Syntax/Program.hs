module Syntax.Program where

import Syntax.Decls
import Syntax.Exprs
import Syntax.Types
import Data.List

data Program a = Program [Decl a] (Expr a) a deriving(Eq, Ord)

instance Show (Program a) where
    show (Program decls body _) = intercalate "\n\n" ((show <$> decls) ++ [show body])

-- | standard library
prelude :: Program ()
prelude = Program decls unit ()
    where
        decls =
            [ dataDecl "Bool" [] [conDecl "True" [], conDecl "False" []]
            , dataDecl "List" [1] [conDecl "Empty" [], conDecl "Cons" [tvar 1, tcon "List" [tvar 1]]]
            , dataDecl "Maybe" [1] [conDecl "Nothing" [], conDecl "Just" [tvar 1]]
            , dataDecl "Either" [1,2] [conDecl "Left" [tvar 1], conDecl "Right" [tvar 2]]
            , varDecl "id" ("x" \. var "x")
            , varDecl "const" ("x" \. "y" \. var "x")
            , varDecl "compose" ("f" \. "g" \. "x" \. var "f" \$ (var "g" \$ var "x"))
            ]

-- | prepend the first program's decls to the second program. Ghetto importing
appendPrograms :: Program a -> Program a -> Program a
appendPrograms (Program decls _ _) (Program decls' body tag) = Program (decls++decls') body tag

-- combinators for program construction

-- | program with just an expression
eProg :: Expr () -> Program ()
eProg e = Program [] e ()

-- | "ghetto import" prelude
withPrelude :: Program () -> Program ()
withPrelude = appendPrograms prelude

exprWithPrelude :: Expr () -> Program ()
exprWithPrelude = appendPrograms prelude . eProg