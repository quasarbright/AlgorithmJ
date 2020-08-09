module Parsing.ParseAst where

import Syntax.Names

{-
I read that in the haskell parser, they don't precisely parse haskell. They parse a superset of the language and try
to convert it to haskell manually afterwards. For example, the parser doesn't distinguish between patterns and expressions.
Later on, I'm going to have to have functions for validating and converting this AST into the "real" AST.

Some noteworthy things:
- Patterns and expressions are parsed as if they were the same
- There is no notion of let vs letrec. decls will have to be grouped/ordered by dependency by a post-processor (type decls too)
- There is no distinction between variable and constructor names in expressions and types
- expression and type applications are represented as lists. you could just foldl1 (,) for binary applications
- (e) is represented as a 1-element tuple. same for types
-}

defaultFixity :: Fixity
defaultFixity = MkFixity LAssoc (9)

-- TODO sections
-- TODO guards
-- | parsable pattern/expression. union of both syntaxes
data Expr a = Var String a -- variable or constructor
             | PInt Integer a
             | PDouble Double a
             | PChar Char a
             | PString String a
             | Wild a -- _
             | OpVar String a -- (+) or (:)
             | Tup [Expr a] a
             | List [Expr a] a
             | App [Expr a] a
             | Binop (Expr a) String Fixity (Expr a) a
             | Annot (Expr a) (Type a) a
             | Or [Expr a] a -- e | e | ... patterns
             | If (Expr a) (Expr a) (Expr a) a
             | Where (Expr a) [Decl a] a
             | Fun (Expr a) (Expr a) a
             | Let [Decl a] (Expr a) a
             | Case (Expr a) [(Expr a, Expr a)] a
             deriving(Eq, Ord, Show)

getTag :: Expr a -> a
getTag e = case e of
    Var _ a -> a
    PInt _ a -> a
    PDouble _ a -> a
    PChar _ a -> a
    PString _ a -> a
    Wild a -> a
    Tup _ a -> a
    List _ a -> a
    App _ a -> a
    OpVar _ a -> a
    Binop _ _ _ _ a -> a
    Annot _ _ a -> a
    Or _ a -> a
    If _ _ _ a -> a
    Where _ _ a -> a
    Fun _ _ a -> a
    Let _ _ a -> a
    Case _ _ a -> a

data Type a = TVar String a
            | TOpVar String a
            | TList (Type a) a
            | TTup [Type a] a
            | TApp [Type a] a
            | TBinop (Type a) String Fixity (Expr a) a
            | TArr (Type a) (Type a) a
            deriving(Eq, Ord, Show)

--data Guard a = Guard (Expr a) (Expr a) a deriving(Eq, Ord, Show)

data Decl a = Binding (Expr a) (Expr a) a
            | AnnotDecl String (Type a) a
            | DataDecl String [String] [Type a] a
            | FixityDecl Fixity String a
            deriving(Eq, Ord, Show)





