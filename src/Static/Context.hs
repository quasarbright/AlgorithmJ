module Static.Context where

import Syntax.Types
import Syntax.Decls
import qualified Data.Set as Set
import Syntax.Names
import Data.List

data ContextItem a =
                   -- type of a variable
                   VarAnnot VName Type
                   -- signature of a value constructor
                 | ConAnnot CName Type
                   -- data type's name, parameters, and cases
                 | DataInfo TCName [TVName] [ConDecl a] a
                 | VarOpAnnot VarOpName Type
                 | ConOpAnnot ConOpName Type
                 deriving(Eq, Ord)

instance Show (ContextItem a) where
    show (VarAnnot name t) = concat[show name," :: ",show t]
    show (ConAnnot name t) = concat[show name," :: ",show t]
    show (DataInfo tName params cases tag) = show (DataDecl tName params cases tag)
    show (VarOpAnnot name t) = concat["(",show name,") :: ",show t]
    show (ConOpAnnot name t) = concat["(",show name,") :: ",show t]

-- | variable annotations, most recent one first
type Context a = [ContextItem a]


-- modification / building

-- | empty context
emptyContext :: Context a
emptyContext = []

-- | add a variable annotation to the context
addVarAnnot :: VName -> Type -> Context a -> Context a
addVarAnnot x t = (VarAnnot x t:)

-- | add several variable annotations to the context
addVarAnnots :: [(VName, Type)] -> Context a -> Context a
addVarAnnots annots ctx = foldr (uncurry addVarAnnot) ctx annots

-- | add a value constructor annotation to the context, creating a constructor name from the string
addVarAnnotStr :: String -> Type -> Context a -> Context a
addVarAnnotStr s = addVarAnnot (MkVName s)

-- | remove the most recent occurrence of the given variable annotation from the context
removeVarAnnot :: Eq a => VName -> Type -> Context a -> Context a
removeVarAnnot x t = delete (VarAnnot x t)

-- | add a value constructor annotation to the context
addConAnnot :: CName -> Type -> Context a -> Context a
addConAnnot c t= (ConAnnot c t:)

-- | add a value constructor annotation to the context, creating a constructor name from the string
addConAnnotStr :: String -> Type -> Context a -> Context a
addConAnnotStr s = addConAnnot (MkCName s)

-- | add the definition of a data type to the context
addDataInfo :: Eq a =>TCName -> [TVName] -> [ConDecl a] -> a -> Context a -> Context a
addDataInfo name params cases a = (DataInfo name params cases a:)

-- | remove the most recent occurrence of the given value constructor annotation from the context
removeConAnnot :: Eq a => CName -> Type -> Context a -> Context a
removeConAnnot c t = delete (ConAnnot c t)

-- | create a context from a list of variable annotations
ctxFromVarAnnots :: Foldable t => t (String, Type) -> Context a
ctxFromVarAnnots = foldr (\ (name, t) ctx -> VarAnnot (MkVName name) t: ctx) []

-- | create a context from a list of value constructor annotations
ctxFromConAnnots :: Foldable t => t (String, Type) -> Context a
ctxFromConAnnots = foldr (\ (name, t) ctx -> ConAnnot (MkCName name) t: ctx) []


-- observations


-- | get the free type variables of all types in the range of the context
getContextFreeVars :: Context a -> Set.Set TVName
getContextFreeVars ctx = Set.unions (getContextItemFreeVars <$> ctx)

-- | get the free type variables in the given context item
getContextItemFreeVars :: ContextItem a -> Set.Set TVName
getContextItemFreeVars (VarAnnot _ t) = getTypeFreeVars t
getContextItemFreeVars (VarOpAnnot _ t) = getTypeFreeVars t
getContextItemFreeVars (ConAnnot _ t) = getTypeFreeVars t
getContextItemFreeVars (ConOpAnnot _ t) = getTypeFreeVars t
getContextItemFreeVars DataInfo{} = Set.empty

-- | get the unbound type variables of the monotype with respect to the context
-- (free variables in the type which aren't free in the context)
getContextMonoTypeFreeVars :: MonoType -> Context a -> Set.Set TVName
getContextMonoTypeFreeVars t ctx = getMonoTypeFreeVars t `Set.difference` getContextFreeVars ctx

-- | look up the type of the given variable
lookupVar :: Context a -> VName -> Maybe Type
lookupVar ctx name = case find predicate ctx of
    Just (VarAnnot _ t) -> Just t
    _ -> Nothing
    where
        predicate (VarAnnot name' _) = name == name'
        predicate _ = False

-- | look up the type of the given variable operator
lookupVarOp :: Context a -> VarOpName -> Maybe Type
lookupVarOp ctx name = case find predicate ctx of
   Just (VarAnnot _ t) -> Just t
   _ -> Nothing
   where
       predicate (VarOpAnnot name' _) = name == name'
       predicate _ = False


-- | look up the type of the given value constructor
lookupCon :: Context a -> CName -> Maybe Type
lookupCon ctx name = case find predicate ctx of
    Just (ConAnnot _ t) -> Just t
    _ -> Nothing
    where
        predicate (ConAnnot name' _) = name == name'
        predicate _ = False

-- | look up the type of the given value constructor operator
lookupConOp :: Context a -> ConOpName -> Maybe Type
lookupConOp ctx name = case find predicate ctx of
    Just (ConAnnot _ t) -> Just t
    _ -> Nothing
    where
        predicate (ConOpAnnot name' _) = name == name'
        predicate _ = False

-- | look up a value constructor's data type's name
lookupConParent :: Context a -> CName -> Maybe TCName
lookupConParent ctx name = do
    t <- lookupCon ctx name
    return (go t)
    where
        go (TMono (TCon name' _)) = name'
        go (TMono (TArr _ ret)) = go (TMono ret)
        go (TScheme _ body) = go body
        go t' = error ("bad type for value constructor: "++show t')

-- | look up the definition of a value constructor
lookupConDef :: Context a -> CName -> Maybe (TCName, [TVName], ConDecl a)
lookupConDef ctx name = do
    tName <- lookupConParent ctx name
    (params, conDecls) <- lookupData ctx tName
    cd <- find (\ (ConDecl name' _ _) -> name == name') conDecls
    return (tName, params, cd)

-- | look up the definition of a data type given its name
lookupData :: Context a -> TCName -> Maybe ([TVName], [ConDecl a])
lookupData ctx name = case find predicate ctx of
    Just (DataInfo _ params cases _) -> Just (params, cases)
    _ -> Nothing
    where
        predicate (DataInfo name' _ _ _) = name == name'
        predicate _ = False