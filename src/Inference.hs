{-
TODO constructor expressions
TODO decls
TODO annotations
TODO pattern matching
TODO lets have pattern LHSs
TODO let f x = ... sugar
TODO fixed expressions
TODO let rec
TODO let rec and
-}

module Inference where

import Exprs
import Types
import qualified UnionFind as UF
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)
import Control.Monad
import Context

data Reason = Inferring Expr
            | Unifying MonoType MonoType
            deriving(Eq, Ord, Show)

data TypeError = Mismatch MonoType MonoType
               | OccursError TVName MonoType
               deriving(Eq, Ord, Show)

data TCState = MkState{ getNameSource :: [TVName], getContext :: Context, getUF :: UF.UnionFind MonoType, getReasons :: [Reason]}

instance Show TCState where
    show s = "context: "++show (getContext s)++"\n\nunion find: "++show (getUF s)++"\n\nreasons: "++show (getReasons s)

instance Eq TCState where
    MkState _ ctx uf reasons == MkState _ ctx' uf' reasons' = (ctx, uf, reasons) == (ctx', uf', reasons')

initialState :: TCState
initialState = MkState nameSource [] UF.empty []

type TypeChecker a = StateT TCState (Either (TypeError, TCState)) a


-- utilities


-- | get a fresh type name
freshName :: TypeChecker TVName
freshName = do
    ~(n:ns) <- getNameSource <$> get
    modify $ \s -> s{getNameSource = ns}
    return n

-- | get a fresh type variable type
freshMonoType :: TypeChecker MonoType
freshMonoType = TVar <$> freshName

-- | throw a type error
throw :: TypeError -> TypeChecker a
throw err = do
    s <- get
    lift (Left (err, s))

-- | throw a mismatch error
mismatch :: MonoType -> MonoType -> TypeChecker a
mismatch a b = throw (Mismatch a b)

-- | Temporarily manipulate the context for the given computation, and restore it afterwards.
-- NOTE: still modifies name source and union-find permanently
localCtx :: (Context -> Context) -> TypeChecker a -> TypeChecker a
localCtx f computation = do
    oldCtx <- getContext <$> get
    modify $ \s -> s{getContext=f (getContext s)}
    result <- computation
    modify $ \s -> s{getContext=oldCtx}
    return result

-- | Temporarily include a variable annotation in the context for the given computation, and restore the original context
-- afterwards.
-- NOTE: still modifies name source and union find permanently
localVarAnnot :: VName -> Type -> TypeChecker a -> TypeChecker a
localVarAnnot x t = localCtx (addVarAnnot x t)

-- | Temporarily add a reason for the given computation, and restore the original reasons afterwards.
-- NOTE: still modifies name source and union find permanently
localReason :: Reason -> TypeChecker a -> TypeChecker a
localReason reason computation = do
    oldReasons <- getReasons <$> get
    modify $ \s -> s{getReasons=reason:oldReasons}
    result <- computation
    modify $ \s -> s{getReasons=oldReasons}
    return result


-- type inference


-- | attempt to unify two types
unify :: MonoType -> MonoType -> TypeChecker ()
unify a b = localReason (Unifying a b) $ do
    a' <- find a
    b' <- find b
    let err = mismatch a' b'
    case (a', b') of
        (TVar name, t) -> unifyHelp name t a' b'
        (t, TVar name) -> unifyHelp name t a' b'
        (TInt, TInt) -> return ()
        (TArr arg ret, TArr arg' ret') -> unify arg arg' >> unify ret ret'
        (TTup tys, TTup tys')
            | length tys == length tys' -> zipWithM_ unify tys tys'
            | otherwise -> err
        (TCon name tys, TCon name' tys')
            | name == name' && length tys == length tys' -> zipWithM_ unify tys tys'
            | otherwise -> err
        (TInt,   _) -> err
        (TArr{}, _) -> err
        (TTup{}, _) -> err
        (TCon{}, _) -> err

-- | unify a type variable and a type
unifyHelp :: TVName -> MonoType -> MonoType -> MonoType -> TypeChecker ()
unifyHelp name t a' b' = do
    -- occurs check
    when (name `elem` getMonoTypeFreeVars t) (throw (OccursError name t))
    a' `union` b'

-- | union two mono types
union :: MonoType -> MonoType -> TypeChecker ()
union a b = do
    uf <- getUF <$> get
    let uf' = UF.union a b uf
    modify $ \s -> s{getUF = uf'}

-- | find the representative of the given type
find :: MonoType -> TypeChecker MonoType
find t = do
    uf <- getUF <$> get
    return (UF.find uf t)

-- | monomorphize a scheme by replacing the quantified variable with a fresh mono type
instantiate :: Type -> TypeChecker MonoType
instantiate (TScheme name body) = do
    tau <- freshMonoType
    instantiate (substituteType name tau body)
instantiate (TMono t) = return t

-- | polymorphize a mono type by quantifying all unbound variables occurring in it
generalize :: MonoType -> TypeChecker Type
generalize t = do
    ctx <- getContext <$> get
    let fvs = getContextMonoTypeFreeVars t ctx
    return $ foldr TScheme (TMono t) fvs

---- | polymorphize a mono type by quantifying all free variables occurring in it (ignores context).
---- use for final generalization
--blindGeneralize :: MonoType -> TypeChecker Type
--blindGeneralize t = do
--    let fvs = getMonoTypeFreeVars t
--    return $ foldr TScheme (TMono t) fvs

-- | simplify a type by repeatedly substituting its free variables for their solutions
simplify :: MonoType -> TypeChecker MonoType
simplify t = do
    t' <- _stepSimplify t
    if t == t' then return t else simplify t'

-- | simplify a type one level by substituting all its free variables for their solutions
_stepSimplify :: MonoType -> TypeChecker MonoType
_stepSimplify t = do
    let fvs = getMonoTypeFreeVars t
    foldM (\ t' name -> liftM3 substituteMonoType (return name) (find (TVar name)) (return t')) t fvs

-- | Infer a type for the given expression
infer :: Expr -> TypeChecker MonoType
infer e = localReason (Inferring e) $
    case e of
        Var name -> do
            ctx <- getContext <$> get
            case lookupVar ctx name of
                Nothing -> error "unbound var"
                Just t -> instantiate t
        EInt{} -> return TInt
        App f x -> do
            fType <- infer f
            xType <- infer x
            retType <- freshMonoType
            unify fType (TArr xType retType)
            return retType
        Lam x body -> do
            argType <- freshMonoType
            retType <- localVarAnnot x (TMono argType) $ infer body
            return $ TArr argType retType
        Let x value body -> do
            valueType <- infer value
            valueType' <- generalize valueType
            localVarAnnot x valueType' $ infer body
        Tup es -> TTup <$> sequence (infer <$> es)

---- | Add a variable annotation to the context (permanently)
--addVarAnnot :: VName -> Type -> TypeChecker ()
--addVarAnnot x t = do
--    modify $ \s -> s{getContext = (x, t):getContext s}
--
---- | remove the (chronologically) last occurrence of a variable annotation from the context (permanently)
--removeVarAnnot :: VName -> Type -> TypeChecker ()
--removeVarAnnot x t = do
--    modify $ \s -> s{getContext = delete (x, t) (getContext s)}

-- | infer a type for the given expression using the initial state. Discard the final state (unless there's an error)
runInference :: Expr -> TCState -> Either (TypeError, TCState) Type
runInference e = evalStateT (infer >=> simplify >=> generalize $ e)

-- | infer a type for the given expression using the initial state. Include the final state.
runInferenceAndState :: Expr -> TCState -> Either (TypeError, TCState) (Type, TCState)
runInferenceAndState e = runStateT (infer >=> simplify >=> generalize $ e)
