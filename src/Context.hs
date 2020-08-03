module Context where

import Exprs(VName(..),)
import Types
import Data.List
import qualified Data.Set as Set

data ContextItem = VarAnnot VName Type deriving(Eq, Ord)

instance Show ContextItem where
    show (VarAnnot name t) = concat[show name," :: ",show t]

-- | variable annotations, most recent one first
type Context = [ContextItem]

-- | add a variable annotation to the context
addVarAnnot :: VName -> Type -> Context -> Context
addVarAnnot x t ctx = VarAnnot x t:ctx

-- | remove the most recent occurrence of the given variable annotation from the context
removeVarAnnot :: VName -> Type -> Context -> Context
removeVarAnnot x t = delete (VarAnnot x t)

-- | create a context from a list of variable annotations
ctxFromList :: Foldable t => t (String, Type) -> Context
ctxFromList = foldr (\ (name, t) ctx -> VarAnnot (MKVName name) t: ctx) []

-- | get the free type variables of all types in the range of the context
getContextFreeVars :: Context -> Set.Set TVName
getContextFreeVars ctx = Set.unions (getContextItemFreeVars <$> ctx)

-- | get the free type variables in the given context item
getContextItemFreeVars :: ContextItem -> Set.Set TVName
getContextItemFreeVars (VarAnnot _ t) = getTypeFreeVars t

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