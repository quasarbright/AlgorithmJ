module Syntax.Patterns where

import Syntax.Types
import Data.List
import Syntax.Names
import Syntax.Literals

data Pattern a = PVar VName a
             | PLiteral (Literal a) a
             | PCon CName [Pattern a] a
             | PTup [Pattern a] a
             | PList [Pattern a] a
             | POr (Pattern a) (Pattern a) a
             | PWild a
             | PAnnot (Pattern a) MonoType a
             deriving(Eq, Ord)

instance Show (Pattern a) where
    showsPrec p pat =
        let p' = case pat of
                PVar{} -> 10
                PLiteral{} -> 10
                PCon{} -> 9
                PTup{} -> 0
                PList{} -> 0
                POr{} -> 3
                PWild{} -> 10
                PAnnot{} -> 1
        in case pat of
            PVar name _ -> shows name
            PLiteral l _ -> shows l
            PCon name [] _ -> shows name
            PCon name pats _ -> showParen (p > p') $ shows name . showString " " . foldr1 (\ a b -> a . showString " " . b) (showsPrec (p'+1) <$> pats)
            PTup pats _ -> showParen True $ showString (intercalate ", " (show <$> pats))
            PList pats _ -> showString $ "["++intercalate ", " (show <$> pats)++"]"
            POr left right _ -> showParen (p > p') $ showsPrec p' left . showString " | " . showsPrec p' right
            PWild _ -> showString "_"
            PAnnot pat' t _ -> showParen (p > p') $ showsPrec p' pat' . showString " :: " . shows t

getPatternTag :: Pattern a -> a
getPatternTag p_ = case p_ of
    PVar _ a -> a
    PLiteral _ a -> a
    PCon _ _ a -> a
    PTup _ a -> a
    PList _ a -> a
    POr _ _ a -> a
    PWild a -> a
    PAnnot _ _ a -> a

-- combinators for constructing patterns

pvar :: String -> Pattern ()
pvar x = PVar (MkVName x) ()

pint :: Int -> Pattern ()
pint n = PLiteral (LInt n ()) ()

pdouble :: Double -> Pattern ()
pdouble d = PLiteral (LDouble d ()) ()

pchar :: Char -> Pattern ()
pchar c = PLiteral (LChar c ()) ()

pstring :: String -> Pattern ()
pstring s = PLiteral (LString s ()) ()

pcon :: String -> [Pattern ()] -> Pattern ()
pcon name pats = PCon (MkCName name) pats ()

ptup :: [Pattern ()] -> Pattern ()
ptup = flip PTup ()

plist :: [Pattern ()] -> Pattern ()
plist = flip PList ()

por :: [Pattern ()] -> Pattern ()
por [] = error "cannot construct empty or pattern"
por pats = foldr1 (\ l r -> POr l r ()) pats

infixl 3 \|
(\|) :: Pattern () -> Pattern () -> Pattern ()
(\|) l r = POr l r ()

pwild :: Pattern ()
pwild = PWild ()

pannot :: Pattern () -> MonoType -> Pattern ()
pannot p t = PAnnot p t ()