import Test.HUnit
import Common
import Syntax.Exprs
import Syntax.Types
import Static.UnionFind
import qualified Parsing.Graph as Graph
import Static.Inference hiding(union, find)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Syntax.Names
import Syntax.Program
import Syntax.Decls
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
    , teq "group" [([1,2,3], 3), ([4,5,6], 6), ([7,8], 8)] (group (foldr (\ (a,b) uf -> union b a uf) empty [(1,2),(3,2),(2,2),(4,5),(5,6),(6,6),(7,8)]))
    ]

graphTests = TestLabel "graph tests" $ TestList
    [ tpass
    , teq "json dependency"
        (Graph.fromList [ (Set.fromList ["array", "json", "object"], Set.fromList ["num"])
                        , (Set.fromList ["array", "json", "object"], Set.fromList ["array", "json", "object"])
                        , (Set.fromList ["num"], Set.fromList ["num"])
                        , (Set.fromList ["array", "json", "object"], Set.fromList ["string"])
                        , (Set.fromList ["program"], Set.fromList ["array", "json", "object"])
                        ])
        (Graph.coalesceSCCs $ Graph.fromList [ ("json", "array")
                                             , ("array", "json")
                                             , ("json", "object")
                                             , ("object", "json")
                                             , ("json", "num")
                                             , ("num", "num")
                                             , ("json", "string")
                                             , ("object", "string")
                                             , ("program", "json")
                                             ])
    , teq "top sort simple"
        [1,2,3,4]
        (Graph.topologicalSort $ Graph.fromList [ (4, 3)
                                                , (3, 2)
                                                , (2, 1)
                                                ])
    , teq "top sort complex (has self-edges)"
        [Set.fromList ["num"], Set.fromList ["string"], Set.fromList ["array", "json", "object"], Set.fromList ["program"]]
        (Graph.topologicalSort $ Graph.fromList [ (Set.fromList ["array", "json", "object"], Set.fromList ["num"])
                                                , (Set.fromList ["array", "json", "object"], Set.fromList ["array", "json", "object"])
                                                , (Set.fromList ["num"], Set.fromList ["num"])
                                                , (Set.fromList ["array", "json", "object"], Set.fromList ["string"])
                                                , (Set.fromList ["program"], Set.fromList ["array", "json", "object"])
                                                ])
    , teq "top sort disconnected"
        [1,2,3,4,9,10,11,12]
        (Graph.topologicalSort $ Graph.fromList [ (12, 11)
                                                , (11, 10)
                                                , (10, 9)
                                                , (4, 3)
                                                , (3, 2)
                                                , (2, 1)
                                                ])
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

tInferProgram :: String -> Program () -> Type -> Test
tInferProgram name p t = teq name (Right t) actual
     where
         actual = case runProgramInference p initialState of
             Left err -> Left err
             Right (t, _) -> Right t

tInferProgramError :: String -> Program () -> StaticError -> Test
tInferProgramError name p err = teq name (Left err) actual
     where
         actual = case runProgramInference p initialState of
             Left (err, _) -> Left err
             Right t -> Right t

tInferProgramWithPrelude :: String -> Program () -> Type -> Test
tInferProgramWithPrelude name p t = teq name (Right t) actual
     where
         actual = case runProgramInference (withPrelude p) initialState of
             Left err -> Left err
             Right (t, _) -> Right t

inferenceTests = TestLabel "inference tests" $ TestList
    [ tpass
    , tInfer "int" (int 1) (TMono tint)
    , tInfer "double" (double 1.0) (TMono tdouble)
    , tInfer "char" (char 'a') (TMono tchar)
    , tInferExprWithPrelude "string" (string "hello") (TMono tstring)
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
--    , tInferErrorWithPrelude "loop" ("x" \. var "x" \$ var "x") (OccursError (MkTVName 9) (tvar 9 \-> tvar 10))
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
    , tInferExprWithPrelude "string cons" (con "Cons" \$ char 'a' \$ string "bcd") (TMono tstring)
    , tInfer "fst with pattern lambda" (elamp (ptup [pvar "x", pwild]) (var "x")) (scheme [1,2] (ttup [tvar 1, tvar 2] \-> tvar 1))
    , tInfer "snd with pattern lambda" (elamp (ptup [pwild, pvar "x"]) (var "x")) (scheme [1,2] (ttup [tvar 1, tvar 2] \-> tvar 2))
    , tInferExprWithPrelude "poly id on rhs of match"
        (ecase (var "id") [(pvar "f", var "f" \$ var "f" \$ unit)])
        (TMono tunit)
    , tInferExprWithPrelude "poly id with let"
        (elet "id" ("x" \. var "x") (var "id" \$ var "id" \$ unit))
        (TMono tunit)
    , tInferError "poly id in lambda fails"
        (("f" \. var "f" \$ var "f" \$ unit) \$ ("x"\.var "x"))
        (OccursError (MkTVName 1) (tvar 1 \-> tvar 2))
    , tInferErrorWithPrelude "variable different types in or pattern"
        (ecase (elist [int 1, int 2, int 3]) [(pcon "Cons" [pvar "x",pwild] \| pcon "Cons" [pwild, pvar "x"], var "x")])
        (Mismatch tint (tlist tint))
    , tInferErrorWithPrelude "mixed list fails" (elist [int 1, unit, char 'c']) (Mismatch tunit tchar)
    , tInferExprWithPrelude "[int -> int, a -> a]" (elist ["x"\.var"x"\::tint\->tint, "x"\.var"x"]) (TMono $ tlist (tint \-> tint))
    , tInferExprWithPrelude "[a -> a, int -> int]" (elist ["x"\.var"x", "x"\.var"x"\::tint\->tint]) (TMono $ tlist (tint \-> tint))
    , tInferExprWithPrelude "list head with pattern lambda"
        (pcon "Cons" [pvar "x",pwild] `elamp` var "x")
        (scheme [1] $ tlist (tvar 1) \-> tvar 1)
    , tInferExprWithPrelude "list tail with pattern lambda"
        (pcon "Cons" [pwild,pvar "xs"] `elamp` var "xs")
        (scheme [1] $ tlist (tvar 1) \-> tlist (tvar 1))
    , tInferExprWithPrelude "list id with pattern lambda"
        (pcon "Cons" [pvar "x",pvar "xs"] `elamp` con "Cons" \$ var "x" \$ var "xs")
        (scheme [1] $ tlist (tvar 1) \-> tlist (tvar 1))
    , tInferExprWithPrelude "match list but return unit"
        (pcon "Cons" [pvar "x", pvar "xs"] `elamp` unit)
        (scheme [1] $ tlist (tvar 1) \-> tunit)
    , tInferExprWithPrelude "let fst (x,_) = x in fst"
        (eletf "fst" [ptup [pvar "x",pwild]] (var "x") (var "fst"))
        (scheme [1,2] $ ttup [tvar 1, tvar 2] \-> tvar 1)
    , tInferExprWithPrelude "let snd (_,x) = x in snd"
        (eletf "snd" [ptup [pwild,pvar "x"]] (var "x") (var "snd"))
        (scheme [1,2] $ ttup [tvar 1, tvar 2] \-> tvar 2)
    , tInferError "loop with let fun"
        (eletf "loop" [pvar "x"] (var "x" \$ var "x") (var "loop"))
        (OccursError (MkTVName 1) (tvar 1 \-> tvar 2))
    , tInferError "y combinator with lef fun"
        (eletf "y" [pvar "f"] (("x"\. var "f" \$ (var "x" \$ var "x")) \$ ("x"\. var "f" \$ (var "x" \$ var "x"))) (var "y"))
        (OccursError (MkTVName 2) (tvar 2 \-> tvar 3))
    , tInferExprWithPrelude "list map"
        (letrec [fbind "map" [pvar "f", pvar "xs"]
            (ecase (var "xs")
                [(pcon "Cons" [pvar "x", pvar "xs"], (var "f" \$ var "x") \: (var "map" \$ var "f" \$ var "xs")),
                 (pcon "Empty" []                  , elist [])])]
        (var "map"))
        (scheme [1,2] $ (tvar 1 \-> tvar 2) \-> tlist (tvar 1) \-> tlist (tvar 2))
    , tInferExprWithPrelude "[1,1,1,1,...]"
        (letrec [vbind "xs" (int 1\:var "xs")] (var "xs"))
        (TMono $ tlist tint)
    , tInferExprWithPrelude "[1,2,1,2,...] via multi letrec"
        (letrec [vbind "ones" (int 1\:var "twos"), vbind "twos" (int 2\:var "ones")] (var "ones"))
        (TMono $ tlist tint)
    , tInferExprWithPrelude "skip every other element via multi letrec"
        (letrec [ fbind "skipSecond" [pcon "Cons" [pvar "x", pvar "xs"]] (var "x"\:(var "skipFirst" \$ var "xs"))
                , fbind "skipFirst"  [pcon "Cons" [pwild,    pvar "xs"]] (var "skipSecond" \$ var "xs")
                ]
        (var "skipFirst"))
        (scheme [1] $ tlist (tvar 1) \-> tlist (tvar 1))
    , tInferExprWithPrelude "map ignore"
        (var "map" \$ (var "const" \$ unit))
        (scheme [1] $ tlist (tvar 1) \-> tlist tunit)
    , tInferExprWithPrelude "prelude append"
        (var "append")
        (scheme [1] $ tlist (tvar 1) \-> tlist (tvar 1) \-> tlist (tvar 1))
    , tInferExprWithPrelude "append 1s mutual recursion"
        (letrec
            [ fbind "appendOnes" [pvar "xs"] (var "append" \$ var "xs" \$ var "ones")
            , vbind "ones" (var "append" \$ elist [int 1] \$ (var "appendOnes" \$ var "ones"))
            ]
        (tup [var "ones", var "appendOnes"]))
        (TMono $ ttup [tlist tint, tlist tint \-> tlist tint])
    , tInferExprWithPrelude "prelude concat"
        (var "concat")
        (scheme [1] $ tlist (tlist (tvar 1)) \-> tlist (tvar 1))
    , tInferExprWithPrelude "cons with multi function"
        (fun [pvar "x", pvar "xs"] (var "x"\:var "xs"))
        (scheme [1] $ tvar 1 \-> tlist (tvar 1) \-> tlist (tvar 1))
    , tInferExprWithPrelude "multi function list map no annot"
        (letrec [fbinds "map"
            [ ([pvar "f", pcon "Cons" [pvar "x", pvar "xs"]], (var "f" \$ var "x") \: (var "map" \$ var "f" \$ var "xs"))
            , ([pwild, pcon "Empty" []], elist [])
            ]]
        (var "map"))
        (scheme [1,2] $ (tvar 1 \-> tvar 2) \-> tlist (tvar 1) \-> tlist (tvar 2))
    , tInferExprWithPrelude "multi function list map with annot"
        (letrec [fbindsAnnot "map" ((tvar 1 \-> tvar 2) \-> tlist (tvar 1) \-> tlist (tvar 2))
            [ ([pvar "f", pcon "Cons" [pvar "x", pvar "xs"]], (var "f" \$ var "x") \: (var "map" \$ var "f" \$ var "xs"))
            , ([pwild, pcon "Empty" []], elist [])
            ]]
        (var "map"))
        (scheme [1,2] $ (tvar 1 \-> tvar 2) \-> tlist (tvar 1) \-> tlist (tvar 2))
    , tInferExprWithPrelude "annotated pattern binding"
        {-
        let
            xs :: [Int]
            ys :: [-1]
            (xs, ys, zs) = ([], [], [])
        in (xs, ys, zs)
        -}
        (eletb (pbindAnnots
            [("xs", tlist tint),("ys", tlist (tvar (-1)))]
            (ptup [pvar "xs", pvar "ys", pvar "zs"]) (tup [elist [], elist [], elist []]))
        (tup [var "xs", var "ys", var "zs"]))
        (scheme [1,2] $ ttup [tlist tint, tlist (tvar 1), tlist (tvar 2)]) -- TODO make this a user type var and have it not simplify
    , tInferProgram "mutually recursive data types"
        (prog
            [ dataDeclGroup [ ("Foo", [1], [conDecl "FEmpty" [], conDecl "Foo" [tcon "Bar" [tvar 1], tcon "Bar" [tunit]]])
                            , ("Bar", [1], [conDecl "BEmpty" [], conDecl "Bar" [tvar 1, tcon "Foo" [tvar 1]]])
                            ]]
            (con "Foo" \$ (con "Bar" \$ int 1 \$ con "FEmpty") \$ (con "Bar" \$ unit \$ con "FEmpty")))
        (TMono (tcon "Foo" [tint]))
    , tInferProgramError "mutually recursive data types error"
      (prog
          [ dataDeclGroup [ ("Foo", [1], [conDecl "FEmpty" [], conDecl "Foo" [tcon "Bar" [tvar 1], tcon "Bar" [tunit]]])
                          , ("Bar", [1], [conDecl "BEmpty" [], conDecl "Bar" [tvar 1, tcon "Foo" [tvar 1]]])
                          ]]
          (con "Foo" \$ (con "Bar" \$ int 1 \$ con "FEmpty") \$ (con "Bar" \$ char 'c' \$ con "FEmpty")))
      (Mismatch tunit tchar)
    , tInferExprWithPrelude "if basic"
        (eif etrue (int 1) (int 2))
        (TMono tint)
    , tInferExprWithPrelude "if with id branches"
        (eif etrue (fun [pvar "x" `pannot` tint] (var "x")) (var "id"))
        (TMono $ tint \-> tint)
    , tInferExprWithPrelude "if with id branches reversed"
        (eif etrue (var "id") (fun [pvar "x" `pannot` tint] (var "x")))
        (TMono $ tint \-> tint)
    , tInferExprWithPrelude "if function wrapper"
        (fun [pvar "cnd", pvar "thn", pvar "els"] (eif (var "cnd") (var "thn") (var "els")))
        (scheme [1] $ tbool \-> tvar 1 \-> tvar 1 \-> tvar 1)
    , tInferErrorWithPrelude "if branches different types"
        (eif etrue (int 1) unit)
        (Mismatch tint tunit)
    , tInferErrorWithPrelude "if cond not bool"
        (eif unit (int 1) (int 2))
        (Mismatch tbool tunit)
    ]
    {-
    TODO test shadowing
    TODO curry and uncurry and flip (in prelude)
    -}
    -- why does pattern matching need generalize, but let needs finalize?

tests = TestList
    [ tpass
    , ufTests
    , graphTests
    , inferenceTests
    ]

main :: IO Counts
main = runTestTT tests