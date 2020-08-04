module Decls where

import Names
--import Exprs
import Types
import Data.List

-- | declaration
data Decl =
    -- | type constructor declaration. describes a discriminated union of product types
    DataDecl
        TCName -- type constructor name
        [TVName] -- type parameter names. length = type constructor arity
        [ConDecl] -- union cases
    deriving(Eq, Ord)

instance Show Decl where
    show d = case d of
        DataDecl name params conDecls -> concat["data ",show name," ",unwords (show <$> params)," = ",intercalate " | " (show <$> conDecls)]

-- | value constructor declaration. describes a product type
data ConDecl = ConDecl CName [MonoType] deriving(Eq, Ord)

instance Show ConDecl where
    show (ConDecl name args) = show name ++ show (tcon " " args) -- delegate to tcon show for parentheses

-- combinators for building decls

dataDecl :: String -> [Integer] -> [ConDecl] -> Decl
dataDecl name params = DataDecl (MkTCName name) (MkTVName <$> params)

conDecl :: String -> [MonoType] -> ConDecl
conDecl name = ConDecl (MkCName name)