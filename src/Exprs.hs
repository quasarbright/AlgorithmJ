module Exprs where

import Data.List

-- | variable name
newtype VName = MkVName{getVName :: String} deriving(Eq, Ord)

-- | value constructor name
newtype CName = MkCName{getCName :: String} deriving(Eq, Ord)

instance Show VName where show = getVName

instance Show CName where show = getCName

data Expr = Var VName
          | Con CName
          | EInt Int
          | Lam VName Expr
          | App Expr Expr
          | Let VName Expr Expr
          | Tup [Expr]
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
        in case e of
            Var name -> shows name
            Con name -> shows name
            EInt n -> shows n
            Lam name body -> showParen (p > p') $ showString "\\" . shows name . showString "." . showsPrec p' body
            App f x -> showParen (p > p') $ showsPrec p' f . showString " " . showsPrec (p' + 1) x
            Let x value body -> showParen (p > p') $ showString "let " . shows x . showString " = " . shows value . showString " in " . shows body
            Tup es -> showParen True $ showString (intercalate ", " (show <$> es))


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

infixl 9 \$
(\$) :: Expr -> Expr -> Expr
(\$) = App

infixr 3 \.
(\.) :: String -> Expr -> Expr
x \. e = Lam (MkVName x) e