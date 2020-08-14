module Parsing.ParseAst where

import Syntax.Names
import qualified Data.Set as Set
import Data.Set(Set)

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
defaultFixity = MkFixity LAssoc 9

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
             | BeginEnd (Expr a) a -- begin e end like parens
             | App [Expr a] a
             | Binop (Expr a) String Fixity (Expr a) a
             | Annot (Expr a) (Type a) a
             | Or [Expr a] a -- e | e | ... patterns
             | If (Expr a) (Expr a) (Expr a) a
             | Where (Expr a) [Decl a] a
             | Fun (Expr a) (Expr a) a
             | Let [Decl a] (Expr a) a
             | Case (Expr a) [(Expr a, Expr a)] a
             deriving(Eq, Ord)

instance Show (Expr a) where
    show e = case e of
        Var name _ -> "(Var "++name++")"
        PInt n _ -> "(Int "++show n++")"
        PDouble d _ -> "(Double "++show d++")"
        PChar c _ -> "(Char "++show c++")"
        PString s _ -> "(String "++show s++")"
        Wild{} -> "Wild"
        OpVar name _ -> "(OpVar "++name++")"
        Tup es _ -> "(Tup "++show es++")"
        List es _ -> "(List "++show es++")"
        BeginEnd e' _ -> "(BeginEnd "++show e'++")"
        App es _ -> "(App "++show es++")"
        Binop left name fx right _ -> "(Binop "++show left++" ("++show name++") ("++show fx++") "++show right++")"
        Annot e' t _ -> "(Annot "++show e'++" "++show t++")"
        Or es _ -> "(Or "++show es++")"
        If cnd thn els _ -> "(If "++show cnd++" "++show thn++" "++show els++")"
        Where e' decls _ -> "(Where "++show e'++" "++show decls++")"
        Fun arg ret _ -> "(Fun "++show arg++" "++show ret++")"
        Let decls body _ -> "(Let "++show decls++" "++show body++")"
        Case e' ms _ -> "(Case "++show e'++" "++show ms++")"

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
    BeginEnd _ a -> a
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

getExprFreeVars :: Expr a -> Set String
getExprFreeVars e_ =
    let letWhere e decls =
           let efvs = Set.difference (getExprFreeVars e) (Set.unions (getDeclBoundVars <$> decls))
               dsfvs = Set.unions (getDeclFreeVars <$> decls)
           in Set.union efvs dsfvs
    in case e_ of
        Var name _ -> Set.singleton name
        OpVar name _ -> Set.singleton name
        PInt{} -> Set.empty
        PDouble{} -> Set.empty
        PChar{} -> Set.empty
        PString{} -> Set.empty
        Wild{} -> Set.empty
        Tup es _ -> Set.unions (getExprFreeVars <$> es)
        List es _ -> Set.unions (getExprFreeVars <$> es)
        BeginEnd e _ -> getExprFreeVars e
        App es _ -> Set.unions (getExprFreeVars <$> es)
        Binop left op _ right _ -> Set.insert op $ Set.unions (getExprFreeVars <$> [left, right])
        Annot e _ _ -> getExprFreeVars e
        Or es _ -> Set.unions (getExprFreeVars <$> es)
        If cnd thn els _ -> Set.unions (getExprFreeVars <$> [cnd, thn, els])
        Where e decls _ -> letWhere e decls
        Fun p body _ -> Set.difference (getExprFreeVars p) (getExprFreeVars body)
        Let decls e _ -> letWhere e decls
        Case e ms _ -> Set.union (getExprFreeVars e) msFVS
            where msFVS = Set.unions [Set.difference (getExprFreeVars p) (getExprFreeVars body) | (p,body) <- ms]

data Type a = TVar String a
            | TOpVar String a
            | TJustList a -- []
            | TList (Type a) a -- [t]
            | TTup [Type a] a
            | TApp [Type a] a
            | TBinop (Type a) String Fixity (Type a) a
            | TArr (Type a) (Type a) a
            deriving(Eq, Ord)

instance Show (Type a) where
    show t_ = case t_ of
        TVar name _ -> "(Var "++name++")"
        TOpVar name _ -> "(OpVar "++name++")"
        TJustList{} -> "TJustList"
        TList t _ -> "(TList "++show t++")"
        TTup ts _ -> "(TTup "++show ts++")"
        TApp ts _ -> "(TApp "++show ts++")"
        TBinop left name fx right _ -> "(TBinop "++show left++" ("++show name++") ("++show fx++") "++show right++")"
        TArr arg ret _ -> "(TArr "++show arg++" "++show ret++")"

getTypeTag :: Type a -> a
getTypeTag t_ = case t_ of
    TVar _ a -> a
    TOpVar _ a -> a
    TList _ a -> a
    TTup _ a -> a
    TApp _ a -> a
    TBinop _ _ _ _ a -> a
    TArr _ _ a -> a
    TJustList a -> a

--data Guard a = Guard (Expr a) (Expr a) a deriving(Eq, Ord, Show)

data Decl a = Binding (Expr a) (Expr a) a
            | AnnotDecl String (Type a) a
            | OpAnnotDecl String (Type a) a
            | DataDecl String [String] [Type a] a
            | FixityDecl Fixity String a
            deriving(Eq, Ord)

instance Show (Decl a) where
    show d_ = case d_ of
        Binding lhs rhs _ -> "(Binding "++show lhs++" "++show rhs++")"
        AnnotDecl name t _ -> "(AnnotDecl "++name++" "++show t++")"
        OpAnnotDecl name t _ -> "(OpAnnotDecl "++name++" "++show t++")"
        DataDecl name params cases _ -> "(DataDecl "++name++" "++show params++" "++show cases++")"
        FixityDecl fx name _ -> "(FixityDecl ("++show fx++") "++show name++")"

getDeclFreeVars :: Decl a -> Set String
getDeclFreeVars d_ = case d_ of
    Binding _ body _ -> getExprFreeVars body -- assume non-recursive
    AnnotDecl{} -> Set.empty
    OpAnnotDecl{} -> Set.empty
    DataDecl{} -> Set.empty
    FixityDecl{} -> Set.empty

getDeclBoundVars :: Decl a -> Set String
getDeclBoundVars d_ = case d_ of
    Binding p _ _ -> getExprFreeVars p -- assume non-recursive
    AnnotDecl{} -> Set.empty
    OpAnnotDecl{} -> Set.empty
    DataDecl{} -> Set.empty
    FixityDecl{} -> Set.empty

data Program a = Program [Decl a] a deriving(Eq, Ord)

instance Show (Program a) where
    show (Program decls _) = unlines (show <$> decls)

