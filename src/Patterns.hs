module Patterns where

import Types
import Data.List
import Names

data Pattern = PVar VName
             | PInt Int -- TODO replace with literal type
             | PCon CName [Pattern]
             | PTup [Pattern]
             | POr Pattern Pattern
             | PWild
             | PAnnot Pattern MonoType
             deriving(Eq, Ord)

instance Show Pattern where
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
            PVar name -> shows name
            PInt n -> shows n
            PCon name [] -> shows name
            PCon name pats -> showParen (p > p') $ shows name . showString " " . foldr1 (\ a b -> a . showString " " . b) (showsPrec (p'+1) <$> pats)
            PTup pats -> showParen True $ showString (intercalate ", " (show <$> pats))
            POr left right -> showParen (p > p') $ showsPrec p' left . showString " | " . showsPrec p' right
            PWild -> showString "_"
            PAnnot pat' t -> showParen (p > p') $ showsPrec p' pat' . showString " :: " . shows t


-- combinators for constructing patterns

pvar :: String -> Pattern
pvar = PVar . MkVName

pint :: Int -> Pattern
pint = PInt

pcon :: String -> [Pattern] -> Pattern
pcon name = PCon (MkCName name)

ptup :: [Pattern] -> Pattern
ptup = PTup

por :: [Pattern] -> Pattern
por [] = error "cannot construct empty or pattern"
por pats = foldr1 POr pats

infixl 3 \|
(\|) :: Pattern -> Pattern -> Pattern
(\|) = POr

pwild :: Pattern
pwild = PWild

pannot :: Pattern -> MonoType -> Pattern
pannot = PAnnot