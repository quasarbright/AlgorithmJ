module Names where

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