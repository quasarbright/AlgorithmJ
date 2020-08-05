module Syntax.Literals where

data Literal a = LInt Int a
               | LDouble Double a
               | LChar Char a
               | LString String a
               deriving(Eq, Ord)

instance Show (Literal a) where
    show l = case l of
        LInt n _ -> show n
        LDouble d _ -> show d
        LChar c _ -> show c -- want the ''
        LString s _ -> show s -- want the ""

