module Common where

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

infixl 1 |$>
(|$>) :: Functor f => f a -> (a -> b) -> f b
x |$> f = f <$> x