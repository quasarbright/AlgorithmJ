module Context where

import Types
import Decls
import qualified Data.Set as Set
import Names
import Data.List

data ContextItem =
                   -- type of a variable
                   VarAnnot VName Type
                   -- signature of a value constructor
                 | ConAnnot CName Type
                   -- data type's name, parameters, and cases
                 | DataInfo TCName [TVName] [ConDecl]
                 deriving(Eq, Ord)

instance Show ContextItem where
    show (VarAnnot name t) = concat[show name," :: ",show t]
    show (ConAnnot name t) = concat[show name," :: ",show t]
    show (DataInfo tName params cases) = show (DataDecl tName params cases)

-- | variable annotations, most recent one first
type Context = [ContextItem]


-- modification / building

-- | empty context
emptyContext :: Context
emptyContext = []

-- | add a variable annotation to the context
addVarAnnot :: VName -> Type -> Context -> Context
addVarAnnot x t = (VarAnnot x t:)

-- | add a value constructor annotation to the context, creating a constructor name from the string
addVarAnnotStr :: String -> Type -> Context -> Context
addVarAnnotStr s = addVarAnnot (MkVName s)

-- | remove the most recent occurrence of the given variable annotation from the context
removeVarAnnot :: VName -> Type -> Context -> Context
removeVarAnnot x t = delete (VarAnnot x t)

-- | add a value constructor annotation to the context
addConAnnot :: CName -> Type -> Context -> Context
addConAnnot c t = (ConAnnot c t:)

-- | add a value constructor annotation to the context, creating a constructor name from the string
addConAnnotStr :: String -> Type -> Context -> Context
addConAnnotStr s = addConAnnot (MkCName s)

-- | add the definition of a data type to the context
addDataInfo :: TCName -> [TVName] -> [ConDecl] -> Context -> Context
addDataInfo name params cases = (DataInfo name params cases:)

-- | remove the most recent occurrence of the given value constructor annotation from the context
removeConAnnot :: CName -> Type -> Context -> Context
removeConAnnot c t = delete (ConAnnot c t)

-- | create a context from a list of variable annotations
ctxFromVarAnnots :: Foldable t => t (String, Type) -> Context
ctxFromVarAnnots = foldr (\ (name, t) ctx -> VarAnnot (MkVName name) t: ctx) []

-- | create a context from a list of value constructor annotations
ctxFromConAnnots :: Foldable t => t (String, Type) -> Context
ctxFromConAnnots = foldr (\ (name, t) ctx -> ConAnnot (MkCName name) t: ctx) []


-- observations


-- | get the free type variables of all types in the range of the context
getContextFreeVars :: Context -> Set.Set TVName
getContextFreeVars ctx = Set.unions (getContextItemFreeVars <$> ctx)

-- | get the free type variables in the given context item
getContextItemFreeVars :: ContextItem -> Set.Set TVName
getContextItemFreeVars (VarAnnot _ t) = getTypeFreeVars t
getContextItemFreeVars (ConAnnot _ t) = getTypeFreeVars t
getContextItemFreeVars DataInfo{} = Set.empty

-- | get the unbound type variables of the monotype with respect to the context
-- (free variables in the type which aren't free in the context)
getContextMonoTypeFreeVars :: MonoType -> Context -> Set.Set TVName
getContextMonoTypeFreeVars t ctx = getMonoTypeFreeVars t `Set.difference` getContextFreeVars ctx

-- | look up the type of the given variable
lookupVar :: Context -> VName -> Maybe Type
lookupVar ctx name = case find predicate ctx of
    Just (VarAnnot _ t) -> Just t
    _ -> Nothing
    where
        predicate (VarAnnot name' _) = name == name'
        predicate _ = False

-- | look up the type of the given value constructor
lookupCon :: Context -> CName -> Maybe Type
lookupCon ctx name = case find predicate ctx of
    Just (ConAnnot _ t) -> Just t
    _ -> Nothing
    where
        predicate (ConAnnot name' _) = name == name'
        predicate _ = False

-- | look up a value constructor's data type's name
lookupConParent :: Context -> CName -> Maybe TCName
lookupConParent ctx name = do
    t <- lookupCon ctx name
    return (go t)
    where
        go (TMono (TCon name' _)) = name'
        go (TMono (TArr _ ret)) = go (TMono ret)
        go (TScheme _ body) = go body
        go t' = error ("bad type for value constructor: "++show t')

-- | look up the definition of a value constructor
lookupConDef :: Context -> CName -> Maybe (TCName, [TVName], ConDecl)
lookupConDef ctx name = do
    tName <- lookupConParent ctx name
    (params, conDecls) <- lookupData ctx tName
    cd <- find (\ (ConDecl name' _) -> name == name') conDecls
    return (tName, params, cd)

-- | look up the definition of a data type given its name
lookupData :: Context -> TCName -> Maybe ([TVName], [ConDecl])
lookupData ctx name = case find predicate ctx of
    Just (DataInfo _ params cases) -> Just (params, cases)
    _ -> Nothing
    where
        predicate (DataInfo name' _ _) = name == name'
        predicate _ = False