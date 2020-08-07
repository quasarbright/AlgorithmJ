module Syntax.Names where

-- | variable name
newtype VName = MkVName{getVName :: String} deriving(Eq, Ord)

instance Show VName where show = getVName

-- | value constructor name
newtype CName = MkCName{getCName :: String} deriving(Eq, Ord)

instance Show CName where show = getCName

-- | Type variable name
newtype TVName = MkTVName{getTVName :: Integer} deriving(Eq, Ord)

instance Show TVName where show name = 't':show (getTVName name)

-- | Type constructor name
newtype TCName = MkTCName{getTCName :: String} deriving(Eq, Ord)

instance Show TCName where show = getTCName

-- | infix operator function
newtype VarOpName = MkVarOpName{getVarOpName :: String} deriving(Eq, Ord)

instance Show VarOpName where show = getVarOpName

-- | infix constructor operator
newtype ConOpName = MkConOpName{getConOpName :: String} deriving(Eq, Ord)

instance Show ConOpName where show = getConOpName

-- | operator associativity
data Assoc = LAssoc | RAssoc | NonAssoc deriving(Eq, Ord, Show)

-- | operator associativity and precedence
data Fixity = MkFixity{getAssoc :: Assoc, getPrecedence :: Int} deriving(Eq, Ord, Show)

data Foo = Foo Int Int