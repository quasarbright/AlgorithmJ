import Test.HUnit
import Common
import Exprs
import Types
import UnionFind
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
    ]

tests = TestList
    [ tpass
    , ufTests
    ]

main :: IO Counts
main = runTestTT tests