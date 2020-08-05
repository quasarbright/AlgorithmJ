module Syntax.Exprs where

import Data.List
import Syntax.Names
import Syntax.Types
import Syntax.Patterns

data Expr a = Var VName a
          | Con CName a
          | EInt Int a -- TODO replace with literal type
          | Lam VName (Expr a) a
          | App (Expr a) (Expr a) a
          | Let VName (Expr a) (Expr a) a
          | Tup [Expr a] a
          | Annot (Expr a) MonoType a
          | Case (Expr a) [(Pattern a, Expr a)] a
          deriving(Eq, Ord)

instance Show (Expr a) where
    showsPrec p e =
        let p' = case e of
                Var{} -> 10
                EInt{} -> 10
                Lam{} -> 3
                App{} -> 9
                Let{} -> 3
                Tup{} -> 10
                Con{} -> 10
                Annot{} -> 1
                Case{} -> 3
        in case e of
            Var name _ -> shows name
            Con name _ -> shows name
            EInt n _ -> shows n
            Lam name body _ -> showParen (p > p') $ showString "\\" . shows name . showString "." . showsPrec p' body
            App f x _ -> showParen (p > p') $ showsPrec p' f . showString " " . showsPrec (p' + 1) x
            Let x value body _ -> showParen (p > p') $ showString "let " . shows x . showString " = " . shows value . showString " in " . shows body
            Tup es _ -> showParen True $ showString (intercalate ", " (show <$> es))
            Annot e' t _ -> showParen (p > p') $ showsPrec p' e' . showString " :: " . shows t
            Case e' ms _ -> showParen (p > p') $ showString "case " . shows e' . showString " of | " . showString (intercalate " | " msStrs)
                where msStrs = [concat[show pat," -> ",show rhs] | (pat, rhs) <- ms] -- TODO prevent dangling case issue


-- combinators for constructing expressions

var :: String -> Expr ()
var x = Var (MkVName x) ()

con :: String -> Expr ()
con c = Con (MkCName c) ()

int :: Int -> Expr ()
int n = EInt n ()

elet :: String -> Expr () -> Expr () -> Expr ()
elet name value body = Let (MkVName name) value body ()

tup :: [Expr ()] -> Expr ()
tup es = Tup es ()

unit :: Expr ()
unit = tup []

ecase :: Expr () -> [(Pattern (), Expr ())] -> Expr ()
ecase e ms = Case e ms ()

infixl 2 \::
(\::) :: Expr () -> MonoType -> Expr ()
(\::) e t = Annot e t ()

infixl 9 \$
(\$) :: Expr () -> Expr () -> Expr ()
(\$) f x = App f x ()

infixr 3 \.
(\.) :: String -> Expr () -> Expr ()
x \. e = Lam (MkVName x) e ()

etrue :: Expr ()
etrue = con "True"

efalse :: Expr ()
efalse = con "False"

elist :: Foldable t => t (Expr ()) -> Expr ()
elist = foldr (\ a b -> con "Cons" \$ a \$ b) (con "Empty")

ejust :: Expr () -> Expr ()
ejust = (con "Just" \$)

enothing :: Expr ()
enothing = con "Nothing"

eleft :: Expr () -> Expr ()
eleft = (con "Left" \$)

eright :: Expr () -> Expr ()
eright = (con "Right" \$)