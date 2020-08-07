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
          | Fun [Pattern a] (Expr a) a
          -- function application
          | App (Expr a) (Expr a) a
          -- let binding
          | Let (Binding a) (Expr a) a
          -- (mutually) recursive let binding(s)
          | LetRec [Binding a] (Expr a) a
          -- tuple
          | Tup [Expr a] a
          -- annotated expression
          | Annot (Expr a) MonoType a
          -- case with matches
          | Case (Expr a) [(Pattern a, Expr a)] a
          -- if then else
          | If (Expr a) (Expr a) (Expr a) a
          -- infix operator call like a + b. bool is whether parenthesized
          | VarOp (Expr a) VarOpName Fixity Bool (Expr a) a
          -- infix operator as a standalone expression. like (+)
          | OpVar VarOpName a
          -- infix value constructor operator call like a : b. bool is whether parenthesized
          | ConOp (Expr a) ConOpName Fixity Bool (Expr a) a
          -- infix value constructor operator as a standalone expression
          | ConVar ConOpName a
          -- infix variable call like a `elem` b. bool is whether parenthesized
          | VarIn (Expr a) VName Fixity Bool (Expr a) a
          -- infix value constructor call like a `Foo` b. bool is whether parenthesized
          | ConIn (Expr a) CName Fixity Bool (Expr a) a
          | VarOpPrefix
          deriving(Eq, Ord)

instance Show (Expr a) where
    showsPrec p e =
        let p' = case e of
                Var{} -> 10
                ELiteral{} -> 10
                Fun{} -> 3
                App{} -> 9
                Let{} -> 3
                LetRec{} -> 3
                Tup{} -> 10
                Con{} -> 10
                Annot{} -> 1
                Case{} -> 3
                If {} -> 4
        in case e of
            Var name _ -> shows name
            Con name _ -> shows name
            ELiteral l _ -> shows l
            Fun pats body _ -> showParen (p > p') $ showString "fun" . showsArgPats pats . showString " -> " . showsPrec p' body
            App f x _ -> showParen (p > p') $ showsPrec p' f . showString " " . showsPrec (p' + 1) x
            Let binding body _ -> showParen (p > p') $ showString "let " . shows binding . showString " in " . shows body
            LetRec bindings body _ -> showParen (p > p') $ showString "let rec" . showString (intercalate " and " (show <$> bindings)) . showString " in " . shows body
            Tup es _ -> showParen True $ showString (intercalate ", " (show <$> es))
            Annot e' t _ -> showParen (p > p') $ showsPrec p' e' . showString " :: " . shows t
            Case e' ms _ -> showParen (p > p') $ showString "case " . shows e' . showString " of | " . showString (intercalate " | " msStrs)
                where msStrs = [concat[show pat," -> ",show rhs] | (pat, rhs) <- ms] -- TODO prevent dangling case issue
            If cnd thn els _ -> showParen (p > p') $ showString "if " . showsPrec p' cnd . showString " then " . showsPrec p' thn . showString " else " . showsPrec p' els


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
elet name value body = Let (vbind name value) body ()

-- | let expression with pattern binding
eletp :: Pattern () -> Expr () -> Expr () -> Expr ()
eletp p value body = Let (pbind p value) body ()

-- | let expression for defining a function (potentially multi-argument)
eletf :: String -> [Pattern ()] -> Expr () -> Expr() -> Expr ()
eletf f args functionBody letBody = Let (fbind f args functionBody) letBody ()

eletb :: Binding () -> Expr () -> Expr ()
eletb b body = Let b body ()

letrec :: [Binding ()] -> Expr () -> Expr ()
letrec bindings body = LetRec bindings body ()

tup :: [Expr ()] -> Expr ()
tup es = Tup es ()

unit :: Expr ()
unit = tup []

ecase :: Expr () -> [(Pattern (), Expr ())] -> Expr ()
ecase e ms = Case e ms ()

eif :: Expr () -> Expr () -> Expr () -> Expr ()
eif cnd thn els = If cnd thn els ()

infixl 2 \::
(\::) :: Expr () -> MonoType -> Expr ()
(\::) e t = Annot e t ()

infixl 9 \$
(\$) :: Expr () -> Expr () -> Expr ()
(\$) f x = App f x ()

-- | lambda with a simple variable argument
infixr 3 \.
(\.) :: String -> Expr () -> Expr ()
x \. e = Fun [pvar x] e ()

-- | lambda with an arbitrary pattern argument
infixr 3 `elamp`
elamp :: Pattern () -> Expr () -> Expr ()
elamp p body = Fun [p] body ()

-- | lambda with many pattern arguments
fun :: [Pattern ()] -> Expr () -> Expr ()
fun args body = Fun args body ()

etrue :: Expr ()
etrue = con "True"

efalse :: Expr ()
efalse = con "False"

elist :: Foldable t => t (Expr ()) -> Expr ()
elist = foldr (\ a b -> con "Cons" \$ a \$ b) (con "Empty")

infixr 5 \:
(\:) :: Expr () -> Expr () -> Expr ()
x \: xs = con "Cons" \$ x \$ xs

ejust :: Expr () -> Expr ()
ejust = (con "Just" \$)

enothing :: Expr ()
enothing = con "Nothing"

eleft :: Expr () -> Expr ()
eleft = (con "Left" \$)

eright :: Expr () -> Expr ()
eright = (con "Right" \$)





data Binding a = PatternBinding [(VName, MonoType)] (Pattern a) (Expr a) a
               | FunctionBinding VName (Maybe MonoType) [([Pattern a], Expr a, a)] a
               deriving(Eq, Ord)

showsArgPats :: (Foldable t, Functor t, Show a) => t a -> String -> String
showsArgPats pats = foldr (\s ss -> showString " " . s . ss) (showString "") (showsPrec 10 <$> pats)

showAnnot :: (Show a1, Show a2) => (a1, a2) -> String
showAnnot (name, t) = unwords[show name, "::", show t]

instance Show (Binding a) where
    show b = case b of
        PatternBinding annots pat value _ -> unwords [annotsStr, show pat,"=",show value]
            where annotsStr = "("++intercalate ", " (showAnnot <$> annots)++")"
        FunctionBinding name mAnnot cases _  -> annotStr ++ intercalate ", " (caseStr <$> cases)
            where
                annotStr = case mAnnot of
                    Nothing -> ""
                    Just t -> unwords[show name,"::",show t,", "]
                caseStr (patterns, body, _) = unwords [show name,showsArgPats patterns "","=",show body]
--        FunctionBinding name patterns (Just t) body _ -> show name ++ unwords [showsArgPats patterns "","::",show t,"=",show body]

-- combinators for constructing bindings

vbind :: String -> Expr () -> Binding ()
vbind name = pbind (pvar name)

pbind :: Pattern () -> Expr () -> Binding ()
pbind = pbindAnnots []

pbindAnnots :: [(String, MonoType)] -> Pattern () -> Expr () -> Binding ()
pbindAnnots annots p value = PatternBinding annots' p value ()
    where
        (names, types) = unzip annots
        annots' = (MkVName <$> names) `zip` types

fbind :: String -> [Pattern ()] -> Expr () -> Binding ()
fbind f args value = fbinds f [(args, value)]

fbindAnnot :: String -> MonoType -> [Pattern ()] -> Expr () -> Binding ()
fbindAnnot f t args value = fbindsAnnot f t [(args, value)]

fbinds :: String -> [([Pattern ()], Expr ())] -> Binding ()
fbinds f cases = FunctionBinding (MkVName f) Nothing [(p, value, ()) | (p, value) <- cases]  ()

fbindsAnnot :: String -> MonoType -> [([Pattern ()], Expr ())] -> Binding ()
fbindsAnnot f t cases = FunctionBinding (MkVName f) (Just t) [(p, value, ()) | (p, value) <- cases]  ()
