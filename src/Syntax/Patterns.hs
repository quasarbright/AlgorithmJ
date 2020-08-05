module Syntax.Patterns where

import Syntax.Types
import Data.List
import Syntax.Names

data Pattern a = PVar VName a
             | PInt Int a -- TODO replace with literal type
             | PCon CName [Pattern a] a
             | PTup [Pattern a] a
             | POr (Pattern a) (Pattern a) a
             | PWild a
             | PAnnot (Pattern a) MonoType a
             deriving(Eq, Ord)

instance Show (Pattern a) where
    showsPrec p pat =
        let p' = case pat of
                PVar{} -> 10
                PInt{} -> 10
                PCon{} -> 9
                PTup{} -> 10
                POr{} -> 3
                PWild{} -> 10
                PAnnot{} -> 1
        in case pat of
            PVar name _ -> shows name
            PInt n _ -> shows n
            PCon name [] _ -> shows name
            PCon name pats _ -> showParen (p > p') $ shows name . showString " " . foldr1 (\ a b -> a . showString " " . b) (showsPrec (p'+1) <$> pats)
            PTup pats _ -> showParen True $ showString (intercalate ", " (show <$> pats))
            POr left right _ -> showParen (p > p') $ showsPrec p' left . showString " | " . showsPrec p' right
            PWild _ -> showString "_"
            PAnnot pat' t _ -> showParen (p > p') $ showsPrec p' pat' . showString " :: " . shows t


-- combinators for constructing patterns

pvar :: String -> Pattern ()
pvar x = PVar (MkVName x) ()

pint :: Int -> Pattern ()
pint = flip PInt ()

pcon :: String -> [Pattern ()] -> Pattern ()
pcon name pats = PCon (MkCName name) pats ()

ptup :: [Pattern ()] -> Pattern ()
ptup = flip PTup ()

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