open Types

open Angstrom

let chainl1 e op =
  let rec go acc =
    (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
  e >>= fun init -> go init



let spaces = skip_while (function ' ' | '\t' -> true | _ -> false)

let lex p = spaces *> p <* spaces

let parens p = char '(' *> p <* char ')'
let true_ = lex @@ string "true" *> return (Value true)
let false_ = lex @@ string "false" *> return (Value false)
let not_ = lex @@ char '!' *> return (fun x -> Not x)
let and_ = lex @@ char '&' *> return (fun x y -> And(x, y))
let or_ = lex @@ char '|' *> return (fun x y -> Or(x, y))
let implies = lex @@ string "=>" *> return (fun x y -> Implies(x, y))
let equivalent = lex @@ string "<=>" *> return (fun x y -> Equivalent(x, y))

let var = lex @@ list 
    [ satisfy (function 'A'..'Z' -> true | _ -> false) >>| String.make 1
    ; take_while (function 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> true | _ -> false)
    ]
  >>| (fun name -> Var(String.concat "" name))




let formula : tformula t =
  fix (fun formula ->
      let factor =
        fix (fun factor -> choice
                [ parens formula
                ; true_ 
                ; false_
                ; var
                ; not_ <*> factor
                ] 
            )
      in
      (chainl1 factor 
         (choice 
            [ and_; or_; implies; equivalent]))
    )

let parse =
  parse_string (formula  <* end_of_input)

let is_error =
  function Ok _ -> false | Error _  -> true
let () =
  assert (parse "true&false" = Ok (And(Value true, Value false)));
  assert (parse "true&(false&true)" = Ok (And(Value true, And(Value false, Value true))));
  assert (parse "T" = Ok (Var "T"));
  assert (parse "T&false" = Ok (And(Var "T", Value false)));
  assert (parse "!T" = Ok (Not (Var "T")));
  assert (parse "T | !T" = Ok (Or(Var "T", Not(Var "T"))));
  assert (parse "!T | T" = Ok (Or(Not (Var "T"), Var "T")));
  assert (parse "V1_&true" = Ok (And(Var "V1_", Value true)));
  assert (parse "V1_ & true" = Ok (And(Var "V1_", Value true)));
  assert (parse "V1 => V2" = Ok (Implies(Var "V1", Var "V2")));
  assert (parse "V1 <=> V2" = Ok (Equivalent(Var "V1", Var "V2")));
  assert (is_error @@ parse "V1 V2" );
