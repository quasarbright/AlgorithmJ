module Common where

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x