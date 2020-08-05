{-
TODO tagging
TODO if tagging, then desugar stuff
TODO investigate eagerness
TODO lets have pattern LHSs
TODO let f x = ... sugar
TODO fixed expressions
TODO let rec
TODO let rec and
TODO guards
-}

module Static.Inference where

import Syntax.Exprs
import Syntax.Types
import qualified Static.UnionFind as UF
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)
import Control.Monad
import Static.Context
import Syntax.Names
import Syntax.Decls
import Syntax.Program
import Syntax.Patterns
import qualified Data.Map as Map
import qualified Data.Set as Set
import Static.Errors

data Reason a = Inferring (Expr a)
            | Unifying MonoType MonoType
            deriving(Eq, Ord, Show)

data TCState a = MkState{ getNameSource :: [TVName], getContext :: Context a, getUF :: UF.UnionFind MonoType, getReasons :: [Reason a]}

instance Show (TCState a) where
    show s = "context: "++show (getContext s)++"\n\nunion find: "++show (getUF s)++"\n\nreasons: "++show (getReasons s)

instance Eq a => Eq (TCState a) where
    MkState _ ctx uf reasons == MkState _ ctx' uf' reasons' = (ctx, uf, reasons) == (ctx', uf', reasons')

initialState :: TCState a
initialState = MkState nameSource [] UF.empty []

type TypeChecker tag ret = StateT (TCState tag) (Either (StaticError, TCState tag)) ret


-- utilities

-- | modify the context (permanently)
modifyContext :: (Context a -> Context a) -> TypeChecker a ()
modifyContext f = modify $ \s -> s{getContext = f . getContext $ s}

-- | get a fresh type name
freshName :: TypeChecker a TVName
freshName = do
    ~(n:ns) <- getNameSource <$> get
    modify $ \s -> s{getNameSource = ns}
    return n

-- | get a fresh type variable type
freshMonoType :: TypeChecker a MonoType
freshMonoType = TVar <$> freshName

-- | get n fresh type variable types
freshMonoTypes :: Int -> TypeChecker a [MonoType]
freshMonoTypes n = replicateM n freshMonoType

-- | throw a type error
throw :: StaticError -> TypeChecker a b
throw err = do
    s <- get
    err' <- simplifyErrorTypes err
    lift (Left (err', s))

-- | simplifies the types mentioned in an error if possible
simplifyErrorTypes :: StaticError -> TypeChecker a StaticError
simplifyErrorTypes err = case err of
    Mismatch expected actual -> do
        v <- freshMonoType
        let t = ttup [v, expected, actual]
        t' <- simplify t
        let ~(TTup [_,expected',actual']) = reduceMonoVars t'
        return (Mismatch expected' actual')
    _ -> return err



-- | throw a mismatch error
mismatch :: MonoType -> MonoType -> TypeChecker a b
mismatch a b = throw (Mismatch a b)

-- | Temporarily manipulate the context for the given computation, and restore it afterwards.
-- NOTE: still modifies name source and union-find permanently
localCtx :: (Context a -> Context a) -> TypeChecker a b -> TypeChecker a b
localCtx f computation = do
    oldCtx <- getContext <$> get
    modify $ \s -> s{getContext=f (getContext s)}
    result <- computation
    modify $ \s -> s{getContext=oldCtx}
    return result

-- | Temporarily include a variable annotation in the context for the given computation, and restore the original context
-- afterwards.
-- NOTE: still modifies name source and union find permanently
localVarAnnot :: VName -> Type -> TypeChecker a b -> TypeChecker a b
localVarAnnot x t = localCtx (addVarAnnot x t)

-- | Temporarily add many variable annotations to the context for the given computation, and restore thr original context
-- afterwards.
localVarAnnots :: Foldable t => t (VName, Type) -> TypeChecker a b -> TypeChecker a b
localVarAnnots annots = localCtx (\ ctx -> (foldr (uncurry addVarAnnot) ctx annots))

-- | Temporarily add a reason for the given computation, and restore the original reasons afterwards.
-- NOTE: still modifies name source and union find permanently
localReason :: Reason a -> TypeChecker a b -> TypeChecker a b
localReason reason computation = do
    oldReasons <- getReasons <$> get
    modify $ \s -> s{getReasons=reason:oldReasons}
    result <- computation
    modify $ \s -> s{getReasons=oldReasons}
    return result


-- type inference


-- | attempt to unify two types
unify :: MonoType -> MonoType -> TypeChecker a ()
unify a b = localReason (Unifying a b) $ do
    a' <- find a
    b' <- find b
    let err = mismatch a' b'
    case (a', b') of
        (TVar{}, TVar{}) -> union a' b'
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
unifyHelp :: TVName -> MonoType -> MonoType -> MonoType -> TypeChecker a ()
unifyHelp name t a' b' = do
    -- occurs check
    when (name `elem` getMonoTypeFreeVars t) (throw (OccursError name t))
    a' `union` b'

-- | union two mono types
union :: MonoType -> MonoType -> TypeChecker a ()
union a b = do
    uf <- getUF <$> get
    let uf' = UF.union a b uf
    modify $ \s -> s{getUF = uf'}

-- | find the representative of the given type
find :: MonoType -> TypeChecker a MonoType
find t = do
    uf <- getUF <$> get
    return (UF.find uf t)

-- | monomorphize a scheme by replacing the quantified variable with a fresh mono type
instantiate :: Type -> TypeChecker a MonoType
instantiate (TScheme name body) = do
    tau <- freshMonoType
    instantiate (substituteType name tau body)
instantiate (TMono t) = return t

-- | polymorphize a mono type by quantifying all unbound variables occurring in it
generalize :: MonoType -> TypeChecker a Type
generalize t = do
    ctx <- getContext <$> get
    uf <- getUF <$> get
    let ctxFvs = getContextFreeVars ctx
    let typesReachableFromCtxFvs = concatMap (UF.toRep uf . TVar) (Set.toList ctxFvs)
    let ctxFvs' = Set.unions (getMonoTypeFreeVars <$> typesReachableFromCtxFvs)
    let tFvs = getMonoTypeFreeVars t
    let fvs = Set.difference tFvs ctxFvs'
    -- TODO heavily test this to try to break it. It might under-generalize
    --  Comes into play in polymorphic pattern matching.
    -- The reason I changed it to this is because with pattern matching, types are decomposed such that in
    -- \pair. case pair of (a,_) -> a   we have pair::t1, a::t2, and in the UF, t1=(t2,t3)
    -- t2 depends on t1, so it's not unbound and should therefore not be quantified.
--    let fvs = getContextMonoTypeFreeVars t ctx -- old way if this ever goes wrong
    return $ foldr TScheme (TMono t) fvs

-- | polymorphize a mono type by quantifying all free variables occurring in it (ignores context).
blindGeneralize :: MonoType -> Type
blindGeneralize t = foldr TScheme (TMono t) fvs
    where fvs = getMonoTypeFreeVars t

-- | simplify a type by repeatedly substituting its free variables for their solutions
simplify :: MonoType -> TypeChecker a MonoType
simplify t = do
    t' <- _stepSimplify t
    if t == t' then return t else simplify t'

-- | simplify a type one level by substituting all its free variables for their solutions
_stepSimplify :: MonoType -> TypeChecker a MonoType
_stepSimplify t = do
    let fvs = getMonoTypeFreeVars t
    foldM (\ t' name -> liftM3 substituteMonoType (return name) (find (TVar name)) (return t')) t fvs

-- | simplify and generalize the mono type
finalizeMonoType :: MonoType -> TypeChecker a Type
finalizeMonoType = simplify >=> return . reduceMonoVars >=> return . blindGeneralize


-- | Infer a type for the given expression
infer :: Expr a -> TypeChecker a MonoType
infer e = localReason (Inferring e) $
    case e of
        Var name _ -> do
            ctx <- getContext <$> get
            case lookupVar ctx name of
                Nothing -> throw (UnboundVar name)
                Just t -> instantiate t
        Con name _ -> do
            ctx <- getContext <$> get
            case lookupCon ctx name of
                Nothing -> throw (UnboundCon name)
                Just t -> instantiate t
        EInt{} -> return TInt
        App f x _ -> do
            fType <- infer f
            xType <- infer x
            retType <- freshMonoType
            unify fType (TArr xType retType)
            return retType
        Lam x body _ -> do
            argType <- freshMonoType
            retType <- localVarAnnot x (TMono argType) $ infer body
            return $ TArr argType retType
        Let x value body _ -> do
            valueType <- infer value
            valueType' <- finalizeMonoType valueType
            localVarAnnot x valueType' $ infer body
        Tup es _ -> TTup <$> mapM infer es
        Annot e' t _ -> check e' t >> return t
        Case e' ms _ -> do
            t <- infer e'
            rhsTypes <- mapM (\ (pat, body) -> processBindingWithBody pat t body) ms
            case rhsTypes of
                [] -> throw EmptyCase
                _ -> zipWithM_ unify rhsTypes (tail rhsTypes)
            return (head rhsTypes)



-- | check an expression against the given mono type
check :: Expr a -> MonoType -> TypeChecker a ()
check e t = do
    t' <- infer e
    unify t t'

-- | determines the mono types of variables when the given pattern is bound to an expression of the given type.
-- For example, (x,y) = (1,id) will output {x:Int,y:(a -> a)} (not forall a . a -> a, just a -> a).
-- NOTE: You must generalize the output variables after calling!
processBinding :: Pattern a -> MonoType -> TypeChecker a (Map.Map VName MonoType)
processBinding pattern t = do
    case pattern of
            PVar name _ -> return $ Map.singleton name t
            PInt{} -> unify tint t >> return Map.empty
            PTup pats _ -> do
                -- assume no name repeats TODO wf
                tvars <- freshMonoTypes (length pats)
                let t' = ttup tvars
                unify t' t
                tvars' <- mapM find tvars -- necessary to prevent everything from being quantified in the end
                Map.unions <$> zipWithM processBinding pats tvars'
            PCon cName pats _ -> do
                ctx <- getContext <$> get
                (tName, params, types) <- case lookupConDef ctx cName of
                    Nothing -> throw (UnboundCon cName)
                    Just (tName, params, ConDecl _ types _) -> return (tName, params, types)
                tvars <- freshMonoTypes (length params)
                let t' = TCon tName tvars
                unify t' t
                tvars' <- mapM find tvars -- necessary to prevent everything from being quantified in the end
                -- replace type parameter placeholders with actual type parameters
                let replacements = zip params tvars'
                let types' = substituteManyMonoType replacements <$> types
                -- recursively zip through the child patterns and the product type's subtypes
                -- assume correct arity TODO wf
                Map.unions <$> zipWithM processBinding pats types'
            POr left right _ -> do
                leftResult <- Map.toAscList <$>  processBinding left t
                rightResult <- Map.toAscList <$> processBinding right t
                -- assume same domains TODO wf
                zipWithM_ unify (snd <$> leftResult) (snd <$> rightResult)
                finalRange <- mapM (find . snd) leftResult
                let finalResult = Map.fromAscList (zip (fst <$> leftResult) (finalRange))
                return finalResult
            PWild _ -> return (Map.empty)
            PAnnot pat t' _ -> do
                unify t' t
                processBinding pat t'

-- | abstraction for when the variables of a binding are only used in a single expression.
-- Like @let p = e in body@ or @case e of ... | p -> body | ...@.
-- NOTE: do NOT use for lambdas because this generalizes all variables' types before checking the body, and lambda arguments
--   need to be mono types
processBindingWithBody :: Pattern a -> MonoType -> Expr a -> TypeChecker a MonoType
processBindingWithBody pat t body = do
    newVarAnnots <- Map.toList <$> processBinding pat t
--    ctx <- getContext <$> get
    generalizedTypes <- mapM (generalize . snd) newVarAnnots --(trace ("newVarAnnots: "++show newVarAnnots++"\ncontext: "++show ctx) newVarAnnots)
    let generalizedVarAnnots = zip (fst <$> newVarAnnots) generalizedTypes
    localVarAnnots generalizedVarAnnots (infer body)

-- | Record the declaration in the program, adding its definitions to the context
processDecl :: Eq a => Decl a -> TypeChecker a ()
processDecl d = case d of
    -- examples:
    -- data D a b =  C A1 ... An   adds the annotation C :: \/a.\/b.A1 -> ... -> An -> D a b   to the context
    -- data D a b = C   adds the annotation C :: \/a.\/b.D a b   to the context (no ->)
    -- data D = C A1 ... An   adds the annotation C :: A1 -> ... -> An -> D to the context (no forall)
    --
    -- Precisely, it quantifies the type parameters and makes each constructor return the data type with all parameters.
    -- Does so for each constructor
    DataDecl typeName params cases tag -> addData >> sequence_ (processCase <$> cases)
            where
                addData = modifyContext (addDataInfo typeName params cases tag) -- didn't want to clutter the line
                -- ctx += C :: \/a.\/b. A1 -> ... -> An -> D a b
                processCase (ConDecl conName args _) = modifyContext $ addConAnnot conName (foldr TScheme (TMono arrType) params)
                    where
                        -- A1 -> ... -> An -> D a b
                        arrType = foldr TArr (TCon typeName (TVar <$> params)) args
    VarDecl name value _ -> do
        -- TODO abstract with let when things get more complicated. maybe inferBinding :: Binding -> Map VName Type or something
        -- careful, should let (id1, id2) = (\x.x, \x.x) result in ids? YES
        valueType <- infer value
        valueType' <- finalizeMonoType valueType
        modifyContext $ addVarAnnot name valueType'

-- | run type inference for a program, returning the generalized body type
inferProgram :: Eq a => Program a -> TypeChecker a Type
inferProgram (Program decls body _) = do
    sequence_ (processDecl <$> decls)
    bodyType <- infer body
    finalizeMonoType bodyType

-- | infer a type for the given expression using the initial state. Discard the final state (unless there's an error)
runInference :: Expr a -> TCState a -> Either (StaticError, TCState a) Type
runInference = evalStateT . (infer >=> finalizeMonoType)

-- | infer a type for the given expression using the initial state. Include the final state.
runInferenceAndState :: Expr a -> TCState a -> Either (StaticError, TCState a) (Type, TCState a)
runInferenceAndState = runStateT . (infer >=> finalizeMonoType)

runProgramInference :: Eq a => Program a -> TCState a -> Either (StaticError, TCState a) (Type, TCState a)
runProgramInference = runStateT . inferProgram
