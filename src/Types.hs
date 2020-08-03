module Types where

import qualified Data.Set as Set
import Data.List(intercalate)

-- | Type variable name
newtype TVName = MkTVName{getTVName :: Integer} deriving(Eq, Ord)

instance Show TVName where show name = 't':show (getTVName name)

-- | Type constructor name
newtype TCName = MkTCName{getTCName :: String} deriving(Eq, Ord)

instance Show TCName where show = getTCName

-- | infinite source of variable names
nameSource :: [TVName]
nameSource = MkTVName <$> [1..]

data MonoType = TVar TVName
              | TInt
              | TArr MonoType MonoType
              | TTup [MonoType]
              | TCon TCName [MonoType]
              deriving(Eq, Ord)

instance Show MonoType where
    showsPrec p t =
        let p' = case t of
                TVar{} -> 10
                TInt{} -> 10
                TArr{} -> 3
                TTup{} -> 10
                TCon{} -> 9
        in case t of
            TVar name -> shows name
            TInt -> showString "Int"
            TArr arg ret -> showParen (p > p') $ showsPrec (p' + 1) arg . showString " -> " . showsPrec p' ret
            TTup tys -> showParen True $ showString (intercalate ", " (show <$> tys))
            TCon name [] -> shows name
            TCon name tys -> showParen (p > p') $ shows name . showString " " . foldr1 (\ a b -> a . showString " " . b) (showsPrec (p'+1) <$> tys)

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
    TTup tys -> Set.unions (getMonoTypeFreeVars <$> tys)
    TCon _ tys -> Set.unions (getMonoTypeFreeVars <$> tys)

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
    TTup tys -> TTup (substituteMonoType name replacement <$> tys)
    TCon conName tys -> TCon conName (substituteMonoType name replacement <$> tys)


-- combinators for constructing types


scheme :: (Foldable t, Functor t) => t Integer -> MonoType -> Type
scheme names t = foldr TScheme (TMono t) (MkTVName <$> names)

tvar :: Integer -> MonoType
tvar name = TVar (MkTVName name)

tint :: MonoType
tint = TInt

ttup :: [MonoType] -> MonoType
ttup = TTup

tunit :: MonoType
tunit = ttup []

tcon :: String -> [MonoType] -> MonoType
tcon name = TCon (MkTCName name)

infixr 3 \->
(\->) :: MonoType -> MonoType -> MonoType
(\->) = TArr