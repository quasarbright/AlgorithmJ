module Syntax.Exprs where

import Data.List
import Syntax.Names
import Syntax.Types
import Syntax.Patterns

data Expr = Var VName
          | Con CName
          | EInt Int -- TODO replace with literal type
          | Lam VName Expr
          | App Expr Expr
          | Let VName Expr Expr
          | Tup [Expr]
          | Annot Expr MonoType
          | Case Expr [(Pattern, Expr)]
          deriving(Eq, Ord)

instance Show Expr where
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
            Var name -> shows name
            Con name -> shows name
            EInt n -> shows n
            Lam name body -> showParen (p > p') $ showString "\\" . shows name . showString "." . showsPrec p' body
            App f x -> showParen (p > p') $ showsPrec p' f . showString " " . showsPrec (p' + 1) x
            Let x value body -> showParen (p > p') $ showString "let " . shows x . showString " = " . shows value . showString " in " . shows body
            Tup es -> showParen True $ showString (intercalate ", " (show <$> es))
            Annot e' t -> showParen (p > p') $ showsPrec p' e' . showString " :: " . shows t
            Case e' ms -> showParen (p > p') $ showString "case " . shows e' . showString " of | " . showString (intercalate " | " msStrs)
                where msStrs = [concat[show pat," -> ",show rhs] | (pat, rhs) <- ms] -- TODO prevent dangling case issue


-- combinators for constructing expressions

var :: String -> Expr
var = Var . MkVName

con :: String -> Expr
con = Con . MkCName

int :: Int -> Expr
int = EInt

elet :: String -> Expr -> Expr -> Expr
elet name = Let (MkVName name)

tup :: [Expr] -> Expr
tup = Tup

unit :: Expr
unit = tup []

ecase :: Expr -> [(Pattern, Expr)] -> Expr
ecase = Case

infixl 2 \::
(\::) :: Expr -> MonoType -> Expr
(\::) = Annot

infixl 9 \$
(\$) :: Expr -> Expr -> Expr
(\$) = App

infixr 3 \.
(\.) :: String -> Expr -> Expr
x \. e = Lam (MkVName x) e

etrue :: Expr
etrue = con "True"

efalse :: Expr
efalse = con "False"

elist :: Foldable t => t Expr -> Expr
elist = foldr (\ a b -> con "Cons" \$ a \$ b) (con "Empty")

ejust :: Expr -> Expr
ejust = (con "Just" \$)

enothing :: Expr
enothing = con "Nothing"

eleft :: Expr -> Expr
eleft = (con "Left" \$)

eright :: Expr -> Expr
eright = (con "Right" \$)