import Test.HUnit
import Common
import Syntax.Exprs
import Syntax.Types
import Static.UnionFind
import Static.Inference hiding(union, find)
import qualified Data.Map as Map
import Syntax.Names
import Syntax.Program
import Syntax.Patterns
import Static.Errors

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

tInfer :: String -> Expr () -> Type -> Test
tInfer name e t = TestList [teq name (Right t) (runInference e initialState), tInferExprWithPrelude name e t]

tInferExprWithPrelude :: String -> Expr () -> Type -> Test
tInferExprWithPrelude name e t = teq name (Right t) actual
    where
        actual = case runProgramInference (exprWithPrelude e) initialState of
            Left err -> Left err
            Right (t, _) -> Right t

tInferError :: String -> Expr () -> StaticError -> Test
tInferError name e err = teq name (Left err) actual
    where
        result = runInference e initialState
        actual = case result of
            Left (err, _) -> Left err
            Right r -> Right r

tInferErrorWithPrelude :: String -> Expr () -> StaticError -> Test
tInferErrorWithPrelude name e err = teq name (Left err) actual
    where
        actual = case runProgramInference (exprWithPrelude e) initialState of
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
    , tInfer "church if" ("cond" \. "thn" \. "els" \. var "cond" \$ var "thn" \$ var "els") (scheme [1,2,3] ((tvar 1 \-> tvar 2 \-> tvar 3) \-> tvar 1 \-> tvar 2 \-> tvar 3))
    , tInfer "apply id" (("x" \. var "x") \$ int 1) (TMono tint)
    , tInfer "use id" (elet "id" ("x" \. var "x")  (var "id" \$ int 1)) (TMono tint)
    , tInferExprWithPrelude "use prelude id" (var "id" \$ int 1) (TMono tint)
    , tInfer "id id" (elet "id" ("x" \. var "x")  (var "id" \$ var "id")) (scheme [1] $ tvar 1 \-> tvar 1)
    , tInferExprWithPrelude "prelude id id" (var "id" \$ var "id") (scheme [1] $ tvar 1 \-> tvar 1)
    , tInfer "use id polymorphically" (elet "id" ("x" \. var "x") (var "id" \$ var "id" \$ var "id" \$ int 1)) (TMono tint)
    , tInferError "loop" ("x" \. var "x" \$ var "x") (OccursError (MkTVName 1) (tvar 1 \-> tvar 2))
    , tInferErrorWithPrelude "loop" ("x" \. var "x" \$ var "x") (OccursError (MkTVName 9) (tvar 9 \-> tvar 10))
    , tInfer "unit" unit (TMono tunit)
    , tInfer "2-tuple" (tup [int 1, unit]) (TMono $ ttup [tint, tunit])
    , tInferExprWithPrelude "true" etrue (TMono tbool)
    , tInferExprWithPrelude "false" efalse (TMono tbool)
    , tInferExprWithPrelude "empty" (elist []) (scheme [1] (tlist (tvar 1)))
    , tInferExprWithPrelude "cons 1 empty" (elist [int 1]) (TMono (tlist tint))
    , tInferExprWithPrelude "cons 1 (cons 2 empty)" (elist [int 1, int 2]) (TMono (tlist tint))
    , tInferExprWithPrelude "cons True empty" (elist [etrue]) (TMono (tlist tbool))
    , tInferExprWithPrelude "nothing" enothing (scheme [1] (tmaybe (tvar 1)))
    , tInferExprWithPrelude "just True" (ejust etrue) (TMono $ tmaybe tbool)
    , tInferExprWithPrelude "just nothing" (ejust enothing) (scheme [1] (tmaybe (tmaybe (tvar 1))))
    , tInferExprWithPrelude "just (just nothing)" (ejust (ejust enothing)) (scheme [1] (tmaybe (tmaybe (tmaybe (tvar 1)))))
    , tInferError "monomorphic used as polymorphic" ("f" \. tup [var "f" \$ int 1, var "f" \$ unit]) (Mismatch tint tunit)
    , tInferError "monomorphic used as polymorphic reversed" ("f" \. tup [var "f" \$ unit, var "f" \$ int 1]) (Mismatch tunit tint)
    , tInferExprWithPrelude "const 2" (var "const" \$ int 2) (scheme [1] $ tvar 1 \-> tint)
    , tInferExprWithPrelude "const id" (var "const" \$ var "id") (scheme [1,2] $ tvar 1 \-> tvar 2 \-> tvar 2)
    , tInferExprWithPrelude "id :: int -> int" (var "id" \:: tint \-> tint) (TMono $ tint \-> tint)
    , tInfer "1 :: int" (int 1 \:: tint) (TMono tint)
    , tInferErrorWithPrelude "1 :: Bool" (int 1 \:: tbool) (Mismatch tbool tint)
    , tInferErrorWithPrelude "id :: int -> Bool" (var "id" \:: tint \-> tbool) (Mismatch tbool tint)
    , tInfer "case 1 of 1 -> ()" (ecase (int 1) [(pint 1, unit)]) (TMono tunit)
    , tInfer "match on tuple" (ecase (tup [int 1, unit]) [(ptup [pvar "n", pwild], var "n")]) (TMono tint)
    , tInfer "fst" ("pair" \. ecase (var "pair") [(ptup [pvar "a", pwild], var "a")]) (scheme [1,2] (ttup [tvar 1, tvar 2] \-> tvar 1))
    , tInfer "snd" ("pair" \. ecase (var "pair") [(ptup [pwild, pvar "a"], var "a")]) (scheme [1,2] (ttup [tvar 1, tvar 2] \-> tvar 2))
    , tInferExprWithPrelude "if with a match" ("cnd"\."thn"\."els"\. ecase (var "cnd") [(pcon "True" [], var "thn"), (pcon "False" [], var "els")]) (scheme [1] $ tbool \-> tvar 1 \-> tvar 1 \-> tvar 1)
    , tInfer "or pattern" ("pair"\. ecase (var "pair") [(ptup [pint 0, pvar "a"] \| ptup [pvar "a", pint 0], var "a")]) (TMono $ ttup [tint, tint] \-> tint)
    , tInferExprWithPrelude "list head" ("xs"\. ecase (var "xs") [(pcon "Cons" [pvar "x", pwild], var "x")]) (scheme [1] $ tlist (tvar 1) \-> tvar 1)
    , tInferExprWithPrelude "list tail" ("xs"\. ecase (var "xs") [(pcon "Cons" [pwild, pvar "xs'"], var "xs'")]) (scheme [1] $ tlist (tvar 1) \-> tlist (tvar 1))
    , tInferExprWithPrelude "list tail shadow" ("xs"\. ecase (var "xs") [(pcon "Cons" [pwild, pvar "xs"], var "xs")]) (scheme [1] $ tlist (tvar 1) \-> tlist (tvar 1))
    , tInferExprWithPrelude "[] -> [] | xs -> [0]"
        ("xs"\. ecase (var "xs") [(pcon "Cons" [pwild, pvar "xs'"], con "Cons" \$ int 0 \$ elist[]), (pcon "Empty" [], elist[])])
        (scheme [1] $ tlist (tvar 1) \-> tlist tint)
    , tInferExprWithPrelude "put 0 at head"
            ("xs"\. ecase (var "xs")
                [ (pcon "Cons" [pwild, pvar "xs'"], con "Cons" \$ int 0 \$ var "xs'")
                , (pcon "Empty" [],                 elist[])
                ])
            (TMono $ tlist tint \-> tlist tint)
    , tInferExprWithPrelude "put 0 at head wildcard"
            ("xs"\. ecase (var "xs")
                [ (pcon "Cons" [pwild, pvar "xs'"], con "Cons" \$ int 0 \$ var "xs'")
                , (pwild,                           var "xs")
                ])
            (TMono $ tlist tint \-> tlist tint)
    , tInferExprWithPrelude "map first element"
            ("f"\."xs"\. ecase (var "xs")
                [ (pcon "Cons" [pvar "x", pvar "xs'"], con "Cons" \$ (var "f" \$ var "x") \$ var "xs'")
                , (pcon "Empty" [],                    var "xs")
                ])
            (scheme [1] $ (tvar 1 \-> tvar 1) \-> tlist (tvar 1) \-> tlist (tvar 1))
    , tInferExprWithPrelude "map first element wildcard"
            ("f"\."xs"\. ecase (var "xs")
                [ (pcon "Cons" [pvar "x", pvar "xs'"], con "Cons" \$ (var "f" \$ var "x") \$ var "xs'")
                , (pwild,                              var "xs")
                ])
            (scheme [1] $ (tvar 1 \-> tvar 1) \-> tlist (tvar 1) \-> tlist (tvar 1))
    , tInferExprWithPrelude "compose" (var "compose") (scheme [1,2,3] $ (tvar 2 \-> tvar 3) \-> (tvar 1 \-> tvar 2) \-> (tvar 1 \-> tvar 3))
    , tInferErrorWithPrelude "use compose backwards"
        (elet "ignore" ("x"\. unit)
        (elet "fst" ("pair" \. ecase (var "pair") [(ptup [pvar "a", pwild], var "a")])
        (var "compose" \$ var "fst" \$ var "ignore")))
        (Mismatch (ttup [tvar 1, tvar 2]) tunit)

    , tInferExprWithPrelude "use compose"
        (elet "ignore" ("x"\. unit)
        (elet "fst" ("pair" \. ecase (var "pair") [(ptup [pvar "a", pwild], var "a")])
        (var "compose" \$ var "ignore" \$ var "fst")))
        (scheme [1,2] (ttup [tvar 1,tvar 2] \-> tunit))
    , tInferExprWithPrelude "maybe bind"
        ("mx"\."f"\.
            ecase (var "mx")
                [ (pcon "Just" [pvar "x"], var "f" \$ var "x")
                , (pcon "Nothing" []     , enothing)])
        (scheme [1,2] (tmaybe (tvar 1) \-> (tvar 1 \-> tmaybe (tvar 2)) \-> tmaybe (tvar 2)))
    , tInferExprWithPrelude "maybe bind accidentally restricted"
        ("mx"\."f"\.
            ecase (var "mx")
                [ (pcon "Just" [pvar "x"], var "f" \$ var "x")
                , (pwild                 , var "mx")]) -- this restricts the generalization of f
        (scheme [1] (tmaybe (tvar 1) \-> (tvar 1 \-> tmaybe (tvar 1)) \-> tmaybe (tvar 1)))
    ]

tests = TestList
    [ tpass
    , ufTests
    , inferenceTests
    ]

main :: IO Counts
main = runTestTT tests