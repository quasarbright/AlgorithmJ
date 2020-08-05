module Common where

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

infixl 1 |$>
(|$>) :: Functor f => f a -> (a -> b) -> f b
x |$> f = f <$> x

-- | wrapper type that changes the behavior of equality.
-- All values of this type are equal (for Eq and Ord).
-- Useful for tagging where expr equality is tag-independent
newtype AllSame a = AllSame a deriving(Show)

instance Eq (AllSame a) where
  _ == _ = True

instance Ord (AllSame a) where
  _ `compare` _ = EQ

class Tagged f where
  getTag :: f a -> a