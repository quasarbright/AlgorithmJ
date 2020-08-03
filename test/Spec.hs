import Test.HUnit
import Common
import Exprs
import Types
import UnionFind
import Inference hiding(union, find)
import qualified Data.Map as Map

teq :: (Eq a, Show a) => String -> a -> a -> Test
teq name a b = TestCase (assertEqual name a b)

tpass :: Test
tpass = TestCase $ assertEqual "pass" True True

ufTests = TestLabel "union find tests" $ TestList
    [ tpass
    , teq "basic find" 1 (find empty 1)
    , teq "union 1..5" (Map.fromList [(1,5),(2,5),(3,5),(4,5),(5,5)]) (foldr (\ (a,b) uf -> union a b uf) empty (zip [1..5] [2..5]))
    , teq "union 1..5 reversed" (Map.fromList [(1,5),(2,5),(3,5),(4,5),(5,5)]) (foldr (\ (a,b) uf -> union b a uf) empty (zip [1..5] [2..5]))
    , teq "find 1..5 efficient" 5 (find (Map.fromList [(1,5),(2,5),(3,5),(4,5),(5,5)]) 2)
    , teq "find 1..5 inefficient" 5 (find (Map.fromList (zip [1..5] [2..5])) 1)
    , teq "find 1..5 1" 5 (find (Map.fromList (zip [1..5] [2..5])) 2)
    , teq "inefficient construction :(" (Map.fromList [(1,5),(2,5),(5,6),(6,6)]) (foldr (\ (a,b) uf -> union b a uf) empty [(5, 6), (1,5), (2,5)])
    ]

tInfer :: String -> Expr -> Type -> Test
tInfer name e t = teq name (Right t) (runInference e initialState)

tInferError :: String -> Expr -> TypeError -> Test
tInferError name e err = teq name (Left err) actual
    where
        result = runInference e initialState
        actual = case result of
            Left (err, _) -> Left err
            Right r -> Right r

inferenceTests = TestLabel "inference tests" $ TestList
    [ tpass
    , tInfer "int" (int 1) (TMono tint)
    , tInfer "let x = 1 in x" (elet "x" (int 1) (var "x")) (TMono tint)
    , tInfer "id" ("x" \. var "x") (scheme [1] (tvar 1 \-> tvar 1))
    , tInfer "church true" ("x" \. "y" \. var "x") (scheme [1, 2] (tvar 1 \-> tvar 2 \-> tvar 1))
    , tInfer "church false" ("x" \. "y" \. var "y") (scheme [1, 2] (tvar 1 \-> tvar 2 \-> tvar 2))
    -- even more general than I thought!
    , tInfer "church if" ("cond" \. "thn" \. "els" \. var "cond" \$ var "thn" \$ var "els") (scheme [2,3,5] ((tvar 2 \-> tvar 3 \-> tvar 5) \-> tvar 2 \-> tvar 3 \-> tvar 5))
    , tInfer "apply id" (("x" \. var "x") \$ int 1) (TMono tint)
    , tInfer "use id" (elet "id" ("x" \. var "x")  (var "id" \$ int 1)) (TMono tint)
    , tInfer "id id" (elet "id" ("x" \. var "x")  (var "id" \$ var "id")) (scheme [3] $ tvar 3 \-> tvar 3)
    , tInfer "use id polymorphically" (elet "id" ("x" \. var "x") (var "id" \$ var "id" \$ var "id" \$ int 1)) (TMono tint)
    , tInferError "loop" ("x" \. var "x" \$ var "x") (OccursError (MkTVName 1) (tvar 1 \-> tvar 2))
    , tInfer "unit" unit (TMono tunit)
    , tInfer "2-tuple" (tup [int 1, unit]) (TMono $ ttup [tint, tunit])
    ]

tests = TestList
    [ tpass
    , ufTests
    , inferenceTests
    ]

main :: IO Counts
main = runTestTT tests