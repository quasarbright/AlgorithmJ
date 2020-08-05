module Syntax.Types where

import qualified Data.Set as Set
import Data.List(intercalate)
import Syntax.Names
import Data.Maybe as Maybe

-- | infinite source of variable names
nameSource :: [TVName]
nameSource = MkTVName <$> [1..]

data MonoType = TVar TVName
              | TInt
              | TDouble
              | TChar
              | TArr MonoType MonoType
              | TTup [MonoType]
              | TCon TCName [MonoType]
              deriving(Eq, Ord)
instance Show MonoType where
    showsPrec p t =
        let p' = case t of
                TVar{} -> 10
                TInt{} -> 10
                TDouble{} -> 10
                TChar{} -> 10
                TArr{} -> 3
                TTup{} -> 10
                TCon{} -> 9
        in case t of
            TVar name -> shows name
            TInt -> showString "Int"
            TDouble -> showString "Double"
            TChar -> showString "Char"
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
    TDouble -> Set.empty
    TChar -> Set.empty
    TArr arg ret -> Set.unions (getMonoTypeFreeVars <$> [arg, ret])
    TTup tys -> Set.unions (getMonoTypeFreeVars <$> tys)
    TCon _ tys -> Set.unions (getMonoTypeFreeVars <$> tys)

substituteType :: TVName -> MonoType -> Type -> Type
substituteType name replacement target = case target of
    TMono t -> TMono $ substituteMonoType name replacement t
    TScheme name' body
        | name == name' -> target
        | otherwise -> TScheme name' (substituteType name replacement body)

-- | substitute a name for a type in the target type
substituteMonoType :: TVName -> MonoType -> MonoType -> MonoType
substituteMonoType name replacement = substituteManyMonoType [(name, replacement)]

-- | simultaneously run several substitutions
substituteManyMonoType :: [(TVName, MonoType)] -> MonoType -> MonoType
substituteManyMonoType substitutions target = case target of
    TVar name -> Maybe.fromMaybe target (lookup name substitutions)
    TInt -> target
    TDouble -> target
    TChar -> target
    TArr arg ret -> TArr (substituteManyMonoType substitutions arg) (substituteManyMonoType substitutions ret)
    TTup tys -> TTup (substituteManyMonoType substitutions <$> tys)
    TCon conName tys -> TCon conName (substituteManyMonoType substitutions <$> tys)

-- | Reduces something like t3 -> t5 to t1 -> t2
reduceMonoVars :: MonoType -> MonoType
reduceMonoVars t = 
    let vars = Set.toList $ getMonoTypeFreeVars t
        vars' = tvar <$> [1..(toInteger $ length vars)]
    in substituteManyMonoType (zip vars vars') t

-- combinators for constructing types


scheme :: (Foldable t, Functor t) => t Integer -> MonoType -> Type
scheme names t = foldr TScheme (TMono t) (MkTVName <$> names)

tvar :: Integer -> MonoType
tvar name = TVar (MkTVName name)

tint :: MonoType
tint = TInt

tdouble :: MonoType
tdouble = TDouble

tchar :: MonoType
tchar = TChar

tstring :: MonoType
tstring = tlist tchar

ttup :: [MonoType] -> MonoType
ttup = TTup

tunit :: MonoType
tunit = ttup []

tcon :: String -> [MonoType] -> MonoType
tcon name = TCon (MkTCName name)

infixr 3 \->
(\->) :: MonoType -> MonoType -> MonoType
(\->) = TArr

tbool :: MonoType
tbool = tcon "Bool" []

tlist :: MonoType -> MonoType
tlist t = tcon "List" [t]

tmaybe :: MonoType -> MonoType
tmaybe t = tcon "Maybe" [t]

teither :: MonoType -> MonoType -> MonoType
teither l r = tcon "Either" [l, r]