module Parsing.ResolveParse where

import qualified Parsing.ParseAst as P

import Syntax.Names
import Syntax.Decls
import Syntax.Exprs
import Syntax.Patterns
import Syntax.Types
import Syntax.Literals
import Syntax.Program

import Static.Errors
import Parsing.ParseUtils(SS, combineSS)

import qualified Data.Char as Char
import qualified Data.Set as Set
--import Data.Set(Set)
import qualified Data.Map as Map
--import Data.Map(Map)
import qualified Parsing.Graph as Graph
--import Parsing.Graph(Graph)
import Data.Maybe(catMaybes)
import Data.Map (Map)

{-
converts a parsing AST to a Syntax AST

- any well-formedness?

- resove fixities before all else

- expr to expr
    - check capitalization
    - check starting with :
    - requires bindings and monotypes
    - reject _ or implement holes
    - reject or
    - where to let (easy)
- expr to pattern
    - check capitalization
    - check starting with :
    - going to have to reject a lot
    - application to pcon. need validation
    -
- expr to function lhs
- type to monotype
    - validate TCon
    - list type
- decl(s) to binding
    - dependency analysis (or letrec everything lol)
        - reordering and grouping of decls
        - would require tagging and renaming or sophisticated namespacing
    - reject data decls inside lets
    - x :: t; y :: t; (x,y) = e to a single binding. going to be annoying
        - requires getting free variables of a pattern
-}

isNameCon :: String -> Bool
isNameCon (hd:_) = hd == ':' || Char.isUpper hd
isNameCon [] = error "empty name?"

nameToReference :: String -> SS -> Expr SS
nameToReference name ss = if isNameCon name then Con (MkCName name) ss else Var (MkVName name) ss

exprToExpr :: P.Expr SS -> Either StaticError (Expr SS)
exprToExpr e_ = case e_ of
    P.Var name a -> return $ nameToReference name a
    P.PInt n a -> return $ ELiteral (LInt (fromInteger n) a) a
    P.PDouble d a -> return $ ELiteral (LDouble d a) a
    P.PChar c a -> return $ ELiteral (LChar c a) a
    P.PString s a -> return $ ELiteral (LString s a) a
    P.Wild a -> Left (InvalidExpr e_ a)
    P.OpVar name a -> return $ nameToReference name a
    P.Tup es a -> do
        es' <- mapM exprToExpr es
        return (Tup es' a)
    P.List es a -> do
        es' <- mapM exprToExpr es
        return (List es' a)
    P.BeginEnd e _ -> exprToExpr e
    P.App es _ -> do
        es' <- mapM exprToExpr es
        return $ foldl1 (\f x -> App f x (combineSS (getTag f) (getTag x))) es'
    P.Binop left name _ right a -> do
        left' <- exprToExpr left
        right' <- exprToExpr right
        let name' = nameToReference name a
        return $ App (App name' left' a) right' a
    P.Annot e t a -> do
        e' <- exprToExpr e
        t' <- typeToMonotype t
        return $ Annot e' t' a
    P.Or _ a -> Left $ InvalidExpr e_ a
    P.If cnd thn els a -> do
        ~[cnd', thn', els'] <- mapM exprToExpr [cnd, thn, els]
        return $ If cnd' thn' els' a
    P.Where{} -> error "todo" -- TODO dependency analysis
    P.Let{}  -> error "todo" -- TODO dependency analysis
    P.Fun lhs body a -> do
        pats <- exprToPatterns lhs
        body' <- exprToExpr body
        return $ Fun pats body' a
    P.Case e ms a -> do
        e' <- exprToExpr e
        ms' <- mapM (\(lhs, rhs) -> (,) <$> exprToPattern lhs <*> exprToExpr rhs) ms
        return $ Case e' ms' a

exprToPattern :: P.Expr SS -> Either StaticError (Pattern SS)
exprToPattern p_ = case p_ of
    P.Var name a
        | Char.isUpper (head name) -> return $ PCon (MkCName name) [] a -- assume app handles actual PCon
        | otherwise -> return $ PVar (MkVName name) a
    P.PInt n a -> return (PLiteral (LInt (fromInteger n) a) a)
    P.PDouble d a -> return (PLiteral (LDouble d a) a)
    P.PChar c a -> return (PLiteral (LChar c a) a)
    P.PString s a -> return (PLiteral (LString s a) a)
    P.Wild a -> return $ PWild a
    P.OpVar{} -> error "todo" -- TODO op var ref patterns
    P.Tup es a -> do
        ps <- mapM exprToPattern es
        return $ PTup ps a
    P.List{} -> error "todo" -- TODO list patterns
    P.BeginEnd e _ -> exprToPattern e
    P.App (P.Var name a:es) a'
        | Char.isUpper (head name) -> do
            ps <- mapM exprToPattern es
            return $ PCon (MkCName name) ps a
        | otherwise -> Left $ InvalidPattern p_ a'
    P.App (P.OpVar{}:_) _ -> error "todo" -- TODO PConOp patterns
    P.Binop left name _ right a -> do
        left' <- exprToPattern left
        right' <- exprToPattern right
        if isNameCon name
        then return (PCon (MkCName name) [left', right'] a)
        else Left $ InvalidPattern p_ a

    P.Annot e t a -> do
        p <- exprToPattern e
        t' <- typeToMonotype t
        return $ PAnnot p t' a
    P.Or es _ -> do
        ps <- mapM exprToPattern es
        return $ foldl1 (\ l r -> POr l r (combineSS (getPatternTag l) (getPatternTag r))) ps
    P.If _ _ _ a -> Left $ InvalidPattern p_ a
    P.App _ a -> Left $ InvalidPattern p_ a
    P.Where _ _ a -> Left $ InvalidPattern p_ a
    P.Fun _ _ a -> Left $ InvalidPattern p_ a
    P.Let _ _ a -> Left $ InvalidPattern p_ a
    P.Case _ _ a -> Left $ InvalidPattern p_ a

-- | for lhs of a function
exprToPatterns :: P.Expr SS -> Either StaticError [Pattern SS]
exprToPatterns = undefined

typeToMonotype :: P.Type SS -> Either StaticError MonoType
typeToMonotype t_ = case t_ of
    P.TVar name _
        | Char.isUpper (head name) -> return $ TCon (MkTCName name) [] -- assume app covers real TCon
        | otherwise -> error "todo" -- TODO string type variables --return $ TVar (MkTVName name)
    P.TOpVar{} -> error "todo" -- TODO type op vars
    P.TList t _ -> tlist <$> typeToMonotype t -- TODO actual list type
    P.TTup ts _ -> ttup <$> mapM typeToMonotype ts
    P.TJustList a -> Left $ InvalidType t_ a-- TODO kind inference stuff
    P.TApp (P.TVar name _:ts) a
        | Char.isUpper (head name) -> TCon (MkTCName name) <$> mapM typeToMonotype ts
        | otherwise -> Left $ InvalidType t_ a
    P.TApp (P.TOpVar{}:_) _ -> error "todo" -- TODO type op vars
    P.TApp _ a -> Left $ InvalidType t_ a
    P.TBinop{} -> error "todo" -- TODO type ops
    P.TArr arg ret _ -> TArr <$> typeToMonotype arg <*> typeToMonotype ret

declsToDecls :: [P.Decl SS] -> Either StaticError [Decl SS]
declsToDecls = undefined

gatherDeclVarAnnotations :: [P.Decl SS] -> Map (P.Decl SS) [(String, P.Type SS)]
gatherDeclVarAnnotations decls = snd $ foldl go ([], Map.empty) decls
    where
        go (annots, ans) d_ =
            case d_ of
                P.AnnotDecl name t _ -> ((name, t):annots, ans)
                P.OpAnnotDecl name t _ -> ((name, t):annots, ans)
                P.Binding{} -> (annots, Map.insert d_ annots ans)
                P.DataDecl{} -> (annots, ans)
                P.FixityDecl{} -> (annots, ans)


-- TODO still have to pair up annotations with decls somehow
groupBindingDecls :: [P.Decl SS] -> [Either [P.Decl SS] (P.Decl SS)]
groupBindingDecls decls =
    let
        boundVarToDecl = Map.fromList [(boundVar, decl) | decl <- decls, boundVar <- Set.toList (P.getDeclBoundVars decl)]
        deps = getDeclDeps `concatMap` decls
        getDeclDeps decl@(P.Binding _ body _) =
            let bodyFVS = P.getExprFreeVars body
                referencedDecls = catMaybes [Map.lookup name boundVarToDecl | name <- Set.toList bodyFVS]
            in [(decl, referencedDecl) | referencedDecl <- referencedDecls]
        getDeclDeps _ = []
        declGraph = Graph.fromList deps
        declGraph' = Graph.coalesceSCCs declGraph
        sortedDeclGroups = Graph.topologicalSort declGraph'
        finalizedDeclGroup group = case Set.toList group of
            [decl]
                | group `elem` Graph.getChildren declGraph' group -> Left (Set.toList group) -- self-edge, recursive
                | otherwise -> Right decl
            _ -> Left (Set.toList group)
        finalizedDeclGroups = finalizedDeclGroup <$> sortedDeclGroups
    in finalizedDeclGroups

declsToBindings :: [P.Decl SS] -> Either StaticError [Binding SS]
declsToBindings = undefined

--declToBinding :: Decl SS -> Either StaticError (Binding SS)
--declToBinding = undefined