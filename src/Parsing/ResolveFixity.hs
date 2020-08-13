module Parsing.ResolveFixity(resolveFixity) where

import Control.Monad.Trans.Reader
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Static.Errors
import Syntax.Names
import Parsing.ParseAst
import Control.Monad.Trans.Class
import Parsing.ParseUtils(SS)

{-
- resolve infix operator precedence and associativity based on fixity declarations.
- resolve expressions, types, patterns
- semantics:
    - example:
        x = 1 + 2
        infixl 6 +
        y = 2 * 3
            where infixl 7 *
    - in a decl group, all fixities are in scope. There should be no duplicates on the same level. TODO wf
    - local fixities override outer ones
-}

type FixityState = Map.Map String Fixity
type FixityResolver a = ReaderT FixityState (Either StaticError) a

-- | get name -> fixity map from a list of decls (shallow/non-recursive)
getFixities :: [Decl SS] -> FixityState
getFixities decls = Map.fromList $ concatMap go decls
    where
        go (FixityDecl fx name _) = [(name, fx)]
        go _ = []

-- | given a default fixity and an operator name, return the name's fixity.
lookupFixity :: Fixity -> String -> FixityResolver Fixity
lookupFixity default_ name = do
    mfx <- Map.lookup name <$> ask
    return $ Maybe.fromMaybe default_ mfx

-- | resolve a program's infix operations
resolveProgram :: Program SS -> FixityResolver (Program SS)
resolveProgram (Program decls a) = Program <$> resolveDecls decls <*> return a

-- | resolve a decl group's infix operations
resolveDecls :: [Decl SS] -> FixityResolver [Decl SS]
resolveDecls decls = mapM go decls
    where
        go decl = local (Map.union localFX) (resolveDecl decl)
        localFX = getFixities decls

-- | used for lets and wheres
resolveDeclsAndBody :: [Decl SS] -> Expr SS -> FixityResolver ([Decl SS], Expr SS)
resolveDeclsAndBody decls e = (,) <$> mapM (go resolveDecl) decls <*> go resolveExpr e
    where
        localFX = getFixities decls
        go comp arg = local (Map.union localFX) (comp arg)

-- | resolve an individual decl's infix operations. assume environment
-- includes all fixities in scope
resolveDecl :: Decl SS -> FixityResolver (Decl SS)
resolveDecl d_ = case d_ of
    -- Since what's being bound depends on fixity resolution, this must precede well-formedness.
    Binding lhs rhs a -> Binding <$> resolveExpr lhs <*> resolveExpr rhs <*> return a
    AnnotDecl name t a -> AnnotDecl name <$> resolveType t <*> return a
    OpAnnotDecl name t a -> OpAnnotDecl name <$> resolveType t <*> return a
    DataDecl name params cases a -> DataDecl name params <$> resolveType `mapM` cases <*> return a
    FixityDecl{} -> return d_

-- | resolve an expr's infix operations.
resolveExpr :: Expr SS -> FixityResolver (Expr SS)
resolveExpr e_ = case e_ of
    Var{} -> return e_
    PInt{} -> return e_
    PDouble{} -> return e_
    PChar{} -> return e_
    PString{} -> return e_
    Wild{} -> return e_
    OpVar{} -> return e_
    Tup es a -> Tup <$> resolveExpr `mapM` es <*> return a
    List es a -> List <$> resolveExpr `mapM` es <*> return a
    BeginEnd e a -> BeginEnd <$> resolveExpr e <*> return a
    App es a -> App <$> resolveExpr `mapM` es <*> return a
    Binop a op fx b tag -> do
        a' <- resolveExpr a
        b' <- resolveExpr b
        fx' <- lookupFixity fx op
        let e' = Binop a' op fx' b' tag
        case e' of
            (Binop (Binop c op1 fx1 d _) op2 fx2 e _) -> do
                -- TODO abstract
                fx1' <- lookupFixity fx1 op1
                fx2' <- lookupFixity fx2 op2
                let ~[prec1, prec2] = getPrecedence <$> [fx1', fx2']
                let ~[assoc1, assoc2] = getAssoc <$> [fx1', fx2']
                if prec1 == prec2 && assoc1 /= assoc2
                    then lift (Left (IrresolvableInfix tag))
                else if prec1 < prec2 || (prec1 == prec2 && assoc1 == RAssoc)
                    then Binop c op1 fx1' <$> resolveExpr (Binop d op2 fx2' e tag) <*> return tag
                else return e'
            _ -> return e'
    Annot e t a -> Annot <$> resolveExpr e <*> resolveType t <*> return a
    Or es a -> Or <$> resolveExpr `mapM` es <*> return a
    If cnd thn els a -> If <$> cnd' <*> thn' <*> els' <*> return a
        where ~[cnd', thn', els'] = resolveExpr <$> [cnd, thn, els]
    Where e decls a -> do
        (decls', e') <- resolveDeclsAndBody decls e
        return $ Where e' decls' a
    Fun lhs rhs a -> Fun <$> resolveExpr lhs <*> resolveExpr rhs <*> return a
    Let decls body a -> do
        (decls', body') <- resolveDeclsAndBody decls body
        return $ Let decls' body' a
    Case e ms a -> do
        e' <- resolveExpr e
        let go (lhs, rhs) = (,) <$> resolveExpr lhs <*> resolveExpr rhs
        ms' <- mapM go ms
        return $ Case e' ms' a

-- | resolve a type's infix operations
resolveType :: Type SS -> FixityResolver (Type SS)
resolveType t_ = case t_ of
    TVar{} -> return t_
    TOpVar{} -> return t_
    TJustList{} -> return t_
    TList t a -> TList <$> resolveType t <*> return a
    TTup ts a -> TTup <$> mapM resolveType ts <*> return a
    TApp ts a -> TApp <$> mapM resolveType ts <*> return a
    TBinop a op fx b tag -> do
        a' <- resolveType a
        b' <- resolveType b
        fx' <- lookupFixity fx op
        let t' = TBinop a' op fx' b' tag
        case t' of
            (TBinop (TBinop c op1 fx1 d _) op2 fx2 e _) -> do
                fx1' <- lookupFixity fx1 op1
                fx2' <- lookupFixity fx2 op2
                let ~[prec1, prec2] = getPrecedence <$> [fx1', fx2']
                let ~[assoc1, assoc2] = getAssoc <$> [fx1', fx2']
                if prec1 == prec2 && assoc1 /= assoc2
                    then lift (Left (IrresolvableInfix tag))
                else if prec1 < prec2 || (prec1 == prec2 && assoc1 == RAssoc)
                    then TBinop c op1 fx1' <$> resolveType (TBinop d op2 fx2' e tag) <*> return tag
                else return t'
            _ -> return t'
    TArr arg ret a -> TArr <$> resolveType arg <*> resolveType ret <*> return a

resolveFixity :: Program SS -> Either StaticError (Program SS)
resolveFixity p = runReaderT (resolveProgram p) Map.empty