type tformula =
  | Value of bool 
  | Var of string 
  | Not of tformula 
  | And of tformula * tformula 
  | Or of tformula * tformula 
  | Implies of tformula * tformula 
  | Equivalent of tformula * tformula
[@@ deriving show]
