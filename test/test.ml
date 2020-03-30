open OUnit2
open BooleanFormulae

let p1 = Var "P1"
let p2 = Var "P2"
let q1 = Var "Q1"
let q2 = Var "Q2"
let f1 = Equivalent ( q1 , q2 )
let f2 = Equivalent ( p1 , p2 )
let ex1 = And ( f1 , f2 )

let vars_tests = 
  let set_equal = assert_equal ~cmp:VarsSet.equal in
  "tests for getVars" >::: 
  [ "no vars" >:: (fun _ ->
        getVars (Value true)
        |> set_equal VarsSet.empty
      )
  ; "simple var" >:: (fun _ ->
        getVars p1
        |> set_equal @@ VarsSet.of_list ["P1"]
      )
  ; "(Q1 <=> Q2) ^ (P1 <=> P2)" >:: (fun _ ->
        getVars ex1
        |> set_equal @@  VarsSet.of_list ["P2"; "P1"; "Q1"; "Q2"]
      )
  ]   




let eval_tests = 
  "tests for evalFormula" >::: 
  [ "constant true" >:: (fun _ -> 
        evalFormula empty_env (Value true)
        |> assert_equal true
      )
  ; "complex formula" >:: (fun _ ->
        evalFormula 
          (env_of_bindings ["P1",false;"P2",false;"Q1",false;"Q2",false])
          ex1
        |> assert_equal true
      )
  ]

let buildDecTree_tests =
  "tests for buildDecTree" >:::
  [ "constant true" >:: (fun _ ->
        buildDecTree (Value true)
        |> assert_equal @@ DecLeaf true
      )
  ; "complex formula" >:: (fun _ ->
        buildDecTree ex1
        |> assert_equal @@
        DecRoot("P1",
                DecRoot("P2",
                        DecRoot("Q1", DecRoot("Q2", DecLeaf true, DecLeaf false),
                                DecRoot("Q2", DecLeaf false, DecLeaf true)),
                        DecRoot("Q1", DecRoot("Q2", DecLeaf false, DecLeaf false),
                                DecRoot("Q2", DecLeaf false, DecLeaf false))),
                DecRoot("P2",
                        DecRoot("Q1", DecRoot("Q2", DecLeaf false, DecLeaf false),
                                DecRoot("Q2", DecLeaf false, DecLeaf false)),
                        DecRoot("Q1", DecRoot("Q2", DecLeaf true, DecLeaf false),
                                DecRoot("Q2", DecLeaf false, DecLeaf true))))
      )
  ]


let isTautology_tests =
  "tests for isTautology" >:::
  [ "constant true" >:: (fun _ ->
        isTautology (Value true)
        |> assert_equal true
      )
  ; "P v not P" >:: (fun _ ->
        isTautology (Or(p1, Not p1))
        |> assert_equal true
      )   
  ; "not not P <=> P" >:: (fun _ ->
        isTautology (Equivalent ((Not (Not p1)), p1))
        |> assert_equal true
      )
  ; "single variable is not a tautology" >:: (fun _ ->
        isTautology p1
        |> assert_equal false
      )
  ]

let areEquivalent_tests =
  "tests for areEquivalent" >:::
  [ "constant true" >:: (fun _ ->
        areEquivalent (Value true) (Value true)
        |> assert_equal true
      )
  ; "true equiv P v not P" >:: (fun _ ->
        areEquivalent (Value true) (Or (p1, Not p1))
        |> assert_equal true
      )
  ; "de Morgan law" >:: (fun _ ->
        areEquivalent (Not(Or (p1, q1))) (And(Not(p1), Not(q1)))
        |> assert_equal true
      )
  ; "true and false are not equiv" >:: (fun _ ->
        areEquivalent (Value true) (Value false)
        |> assert_equal false
      )
  ]


let _ = run_test_tt_main @@ test_list
    [ vars_tests
    ; eval_tests
    ; buildDecTree_tests
    ; isTautology_tests
    ; areEquivalent_tests
    ]
