module Syntax.Program where

import Syntax.Decls
import Syntax.Exprs
import Syntax.Types
import Syntax.Patterns
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
--            , varDecl "id" ("x" \. var "x")
--            , varDecl "const" ("x" \. "y" \. var "x")
--            , varDecl "compose" ("f" \. "g" \. "x" \. var "f" \$ (var "g" \$ var "x"))
            , funDecl "id" [pvar "x"] (var "x")
            , funDecl "const" [pvar "x", pwild] (var "x")
            , funDecl "compose" [pvar "f", pvar "g", pvar "x"] (var "f" \$ (var "g" \$ var "x"))
            , declGroup [fbind "map" [pvar "f", pvar "xs"]
                (ecase (var "xs")
                    [(pcon "Cons" [pvar "x", pvar "xs"], (var "f" \$ var "x") \: (var "map" \$ var "f" \$ var "xs")),
                     (pcon "Empty" []                  , elist [])])]
            , declGroup [fbind "append" [pvar "xs", pvar "xs'"]
                (ecase (var "xs")
                    [(pcon "Cons" [pvar "x", pvar "xs"], (var "x" \: (var "append" \$ var "xs" \$ var "xs'"))),
                     (pcon "Empty" [], var "xs'")])]
            , declGroup [fbind "concat" [pvar "lists"]
                (ecase (var "lists")
                    [(pcon "Cons" [pvar "xs", pvar "lists"], var "append" \$ var "xs" \$ (var "concat" \$ var "lists")),
                     (pcon "Empty" [], elist [])])]
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