module Types where

newtype TVName = MkTVName{getTVName :: Integer} deriving(Eq, Ord)

instance Show TVName where show name = 't':show (getTVName name)

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
          | TScheme TVName MonoType
          deriving(Eq, Ord)

instance Show Type where
    show (TMono t) = show t
    show (TScheme name t) = concat ["\\/",show name,".",show t]

