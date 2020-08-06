module Syntax.Exprs where

import Data.List
import Syntax.Names
import Syntax.Types
import Syntax.Patterns
import Syntax.Literals

data Expr a = Var VName a
          -- value constructor. like "Cons"
          | Con CName a
          -- literal. like "1" or "'c'" or "1.9"
          | ELiteral (Literal a) a
          -- lambda expression
          | Lam (Pattern a) (Expr a) a
          -- function application
          | App (Expr a) (Expr a) a
          -- let binding
          | Let (Binding a) (Expr a) a
          -- tuple
          | Tup [Expr a] a
          -- annotated expression
          | Annot (Expr a) MonoType a
          | Case (Expr a) [(Pattern a, Expr a)] a
          deriving(Eq, Ord)

instance Show (Expr a) where
    showsPrec p e =
        let p' = case e of
                Var{} -> 10
                ELiteral{} -> 10
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
            ELiteral l _ -> shows l
            Lam name body _ -> showParen (p > p') $ showString "\\" . shows name . showString "." . showsPrec p' body
            App f x _ -> showParen (p > p') $ showsPrec p' f . showString " " . showsPrec (p' + 1) x
            Let binding body _ -> showParen (p > p') $ showString "let " . shows binding . showString " in " . shows body
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
int n = ELiteral (LInt n ()) ()

double :: Double -> Expr ()
double d = ELiteral (LDouble d ()) ()

char :: Char -> Expr ()
char c = ELiteral (LChar c ()) ()

string :: String -> Expr ()
string s = ELiteral (LString s ()) ()

-- | let expression with simple variable binding
elet :: String -> Expr () -> Expr () -> Expr ()
elet name value body = Let (PatternBinding (pvar name) value ()) body ()

-- | let expression with pattern binding
eletp :: Pattern () -> Expr () -> Expr () -> Expr ()
eletp p value body = Let (PatternBinding p value ()) body ()

-- | let expression for defining a function (potentially multi-argument)
eletf :: String -> [Pattern ()] -> Expr () -> Expr() -> Expr ()
eletf f args functionBody letBody = Let (FunctionBinding (MkVName f) args Nothing functionBody ()) letBody ()

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

-- | lambda with a simple variable argument
infixr 3 \.
(\.) :: String -> Expr () -> Expr ()
x \. e = Lam (pvar x) e ()

-- | lambda with an arbitrary pattern argument
infixr 3 `elamp`
elamp :: Pattern () -> Expr () -> Expr ()
elamp p body = Lam p body ()

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





data Binding a = PatternBinding (Pattern a) (Expr a) a
               | FunctionBinding VName [Pattern a] (Maybe MonoType) (Expr a) a
               deriving(Eq, Ord)

instance Show (Binding a) where
    show b = case b of
        PatternBinding pat value _ -> unwords [show pat,"=",show value]
        FunctionBinding name patterns Nothing body _ -> unwords [show name,unwords (show <$> patterns),"=",show body]
        FunctionBinding name patterns (Just t) body _ -> unwords [show name,unwords (show <$> patterns),"::",show t,"=",show body]