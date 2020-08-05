module Syntax.Decls where

import Syntax.Names
import Syntax.Exprs
import Syntax.Types
import Data.List

-- | declaration
data Decl a =
    -- | type constructor declaration. describes a discriminated union of product types
    DataDecl
        TCName -- type constructor name
        [TVName] -- type parameter names. length = type constructor arity
        [ConDecl a] -- union cases
        a -- tag
    | VarDecl
        VName -- variable name
        (Expr a) -- value
        a
    deriving(Eq, Ord)

instance Show (Decl a) where
    show d = case d of
        DataDecl name params conDecls _ -> concat["data ",show name," ",unwords (show <$> params)," = ",intercalate " | " (show <$> conDecls)]
        VarDecl name value _ -> concat[show name," = ",show value]

-- | value constructor declaration. describes a product type
data ConDecl a = ConDecl CName [MonoType] a deriving(Eq, Ord)

instance Show (ConDecl s) where
    show (ConDecl name args _) = show name ++ show (tcon " " args) -- delegate to tcon show for parentheses

-- combinators for building decls

dataDecl :: String -> [Integer] -> [ConDecl ()] -> Decl ()
dataDecl name params cds  = DataDecl (MkTCName name) (MkTVName <$> params) cds ()

conDecl :: String -> [MonoType] -> ConDecl ()
conDecl name tys = ConDecl (MkCName name) tys ()

varDecl :: String -> Expr () -> Decl ()
varDecl name value = VarDecl (MkVName name) value ()