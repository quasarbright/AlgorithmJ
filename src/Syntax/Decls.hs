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
    | DataDeclGroup
        [(TCName, [TVName], [ConDecl a], a)]
        a
    deriving(Eq, Ord)

showData :: (Show a1, Show a2, Show a3) => a1 -> [a2] -> [a3] -> [Char]
showData name params conDecls = concat[show name," ",unwords (show <$> params)," = ",intercalate " | " (show <$> conDecls)]

instance Show (Decl a) where
    show d = case d of
        DataDecl name params conDecls _ -> "data " ++ showData name params conDecls
        BindingDecl binding _ -> "let "++show binding
        BindingDeclGroup bindings _ -> "let rec "++intercalate " and " (show <$> bindings)
        DataDeclGroup dataDefs _ -> "data " ++ intercalate " and " defStrs
            where defStrs = [showData name params conDecls | (name, params, conDecls, _) <- dataDefs]

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
varDecl name value = BindingDecl (vbind name value) ()

patDecl :: Pattern () -> Expr () -> Decl ()
patDecl p value = BindingDecl (pbind p value) ()

funDecl :: String -> [Pattern ()] -> Expr () -> Decl ()
funDecl f args body = BindingDecl (fbind f args body) ()

declGroup :: [Binding ()] -> Decl ()
declGroup bindings = BindingDeclGroup bindings ()

dataDeclGroup :: [(String, [Integer], [ConDecl ()])] -> Decl ()
dataDeclGroup dataDecls = DataDeclGroup [(MkTCName name, MkTVName <$> params, conDecls, ()) | (name, params, conDecls) <- dataDecls] ()