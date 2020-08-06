module Syntax.Decls where

import Syntax.Names
import Syntax.Exprs
import Syntax.Patterns
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
    | BindingDecl
        (Binding a)
        a
    | BindingDeclGroup -- mutually recursive
        [Binding a]
        a
    deriving(Eq, Ord)

instance Show (Decl a) where
    show d = case d of
        DataDecl name params conDecls _ -> concat["data ",show name," ",unwords (show <$> params)," = ",intercalate " | " (show <$> conDecls)]
        BindingDecl binding _ -> "let "++show binding
        BindingDeclGroup bindings _ -> "let rec "++intercalate " and " (show <$> bindings)

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
varDecl name value = BindingDecl (PatternBinding (pvar name) value ()) ()

varDeclp :: Pattern () -> Expr () -> Decl ()
varDeclp p value = BindingDecl (PatternBinding p value ()) ()

patDecl :: Pattern () -> Expr () -> Decl ()
patDecl p value = BindingDecl (PatternBinding p value ()) ()

funDecl :: String -> [Pattern ()] -> Expr () -> Decl ()
funDecl f args body = BindingDecl (FunctionBinding (MkVName f) args Nothing body ()) ()