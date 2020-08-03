module Types where

import qualified Data.Set as Set

newtype TVName = MkTVName{getTVName :: Integer} deriving(Eq, Ord)

instance Show TVName where show name = 't':show (getTVName name)

-- | infinite source of variable names
nameSource :: [TVName]
nameSource = MkTVName <$> [1..]

data MonoType = TVar TVName
              | TInt
              | TArr MonoType MonoType
              deriving(Eq, Ord)

instance Show MonoType where
    showsPrec p t =
        let p' = case t of
                TVar{} -> 10
                TInt{} -> 10
                TArr{} -> 3
        in case t of
            TVar name -> shows name
            TInt -> showString "Int"
            TArr arg ret -> showParen (p > p') $ showsPrec (p' + 1) arg . showString " -> " . showsPrec p' ret

data Type = TMono MonoType
          | TScheme TVName Type
          deriving(Eq, Ord)

instance Show Type where
    show (TMono t) = show t
    show (TScheme name t) = concat ["\\/",show name,".",show t]

getTypeFreeVars :: Type -> Set.Set TVName
getTypeFreeVars t = case t of
    TMono t' -> getMonoTypeFreeVars t'
    TScheme name body -> Set.delete name (getTypeFreeVars body)

getMonoTypeFreeVars :: MonoType -> Set.Set TVName
getMonoTypeFreeVars t = case t of
    TVar name -> Set.singleton name
    TInt -> Set.empty
    TArr arg ret -> Set.unions (getMonoTypeFreeVars <$> [arg, ret])

substituteType :: TVName -> MonoType -> Type -> Type
substituteType name replacement target = case target of
    TMono t -> TMono $ substituteMonoType name replacement t
    TScheme name' body
        | name == name' -> target
        | otherwise -> TScheme name' (substituteType name replacement body)

substituteMonoType :: TVName -> MonoType -> MonoType -> MonoType
substituteMonoType name replacement target = case target of
    TVar name'
        | name == name' -> replacement
        | otherwise -> target
    TInt -> target
    TArr arg ret -> TArr (substituteMonoType name replacement arg) (substituteMonoType name replacement ret)

scheme :: (Foldable t, Functor t) => t Integer -> MonoType -> Type
scheme names t = foldr TScheme (TMono t) (MkTVName <$> names)

tvar :: Integer -> MonoType
tvar name = TVar (MkTVName name)

tint :: MonoType
tint = TInt

infixr 3 \->
(\->) :: MonoType -> MonoType -> MonoType
(\->) = TArr