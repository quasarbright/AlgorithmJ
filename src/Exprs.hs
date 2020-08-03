module Exprs where

newtype VName = MKVName{getVName :: String} deriving(Eq, Ord)

instance Show VName where show = getVName

data Expr = Var VName
          | EInt Int
          | Lam VName Expr
          | App Expr Expr
          | Let VName Expr Expr
          deriving(Eq, Ord)

instance Show Expr where
    showsPrec p e =
        let p' = case e of
                Var{} -> 10
                EInt{} -> 10
                Lam{} -> 3
                App{} -> 9
                Let{} -> 3
        in case e of
            Var name -> shows name
            EInt n -> shows n
            Lam name body -> showParen (p > p') $ showString "\\" . shows name . showString "." . showsPrec p' body
            App f x -> showParen (p > p') $ showsPrec p' f . showString " " . showsPrec (p' + 1) x
            Let x value body -> showParen (p > p') $ showString "let " . shows x . showString " = " . shows value . showString " in " . shows body

var :: String -> Expr
var = Var . MKVName

int :: Int -> Expr
int = EInt

elet :: String -> Expr -> Expr -> Expr
elet name value body = Let (MKVName name) value body

infixl 9 \$
(\$) :: Expr -> Expr -> Expr
(\$) = App

infixr 3 \.
(\.) :: String -> Expr -> Expr
x \. e = Lam (MKVName x) e