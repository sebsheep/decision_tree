open Types


let show_tformula = show_tformula

module Parser = Parser

type decTree =
  | DecLeaf of bool
  | DecRoot of string * decTree * decTree


module VarsSet = Set.Make(String)
module StringMap = Map.Make(String)


let rec getVars (formula : tformula) : VarsSet.t =
  match formula with
  | Value _ -> VarsSet.empty
  | Var var ->  VarsSet.singleton var
  | Not f -> getVars f
  | And (f1, f2) -> VarsSet.union (getVars f1) (getVars f2)
  | Or (f1, f2) -> VarsSet.union (getVars f1) (getVars f2)
  | Implies (f1, f2) -> VarsSet.union (getVars f1) (getVars f2)
  | Equivalent (f1, f2) -> VarsSet.union (getVars f1) (getVars f2)



type env = bool StringMap.t

let empty_env = StringMap.empty

let env_of_bindings bindings =
  bindings |> List.to_seq |> StringMap.of_seq


let rec evalFormula (env : env) (formula : tformula) : bool =
  match formula with 
  | Value b -> b
  | Var var -> StringMap.find var env
  | Not f -> not @@ evalFormula env f
  | And (f1, f2) -> (evalFormula env f1) && (evalFormula env f2)
  | Or (f1, f2) -> (evalFormula env f1) || (evalFormula env f2)
  | Implies (f1, f2) -> not (evalFormula env f1) ||  (evalFormula env f2)
  | Equivalent (f1, f2) -> 
    let p = evalFormula env f1 in
    let q = evalFormula env f2 in
    (p && q) || (not p && not q)


let rec buildDecTree' (env : env) (formula : tformula) (vars : string list) : decTree =
  match vars with
  | [] -> DecLeaf (evalFormula env formula)
  | var::othersVars ->
    DecRoot ( var
            , buildDecTree' (StringMap.add var false env) formula othersVars
            , buildDecTree' (StringMap.add var true env) formula othersVars )

let buildDecTree (formula : tformula) : decTree =
  buildDecTree' StringMap.empty formula (VarsSet.elements (getVars formula))


type bddNode =  
  | BddLeaf of bool
  | BddNode of string * int * int
[@@deriving ord]


module BddGraph = Map.Make(struct 
    type t = bddNode
    let compare = compare_bddNode
  end
  )

type bddGraph = int * int BddGraph.t

let emptyBddGraph : bddGraph = (0, BddGraph.empty)

let addNode (node : bddNode) (((maxIndex, graph) as bddG) : bddGraph) : (int * bddGraph) =
  match BddGraph.find_opt node graph  with
  | None ->
    let newMaxIndex = maxIndex + 1 in
    (newMaxIndex, (newMaxIndex, BddGraph.add node newMaxIndex graph))
  | Some index -> (index, bddG)            


let rec buildBdd' 
    (formula : tformula) 
    (vars: string list) 
    (env: env) 
    (bddG: bddGraph)
  : (int * bddGraph) =
  match vars with
  | [] -> addNode (BddLeaf (evalFormula env formula)) bddG
  | var:: otherVars ->
    let (i1, g1) = buildBdd' formula otherVars (StringMap.add var false env) bddG in
    let (i2, g2) = buildBdd' formula otherVars (StringMap.add var true env) g1 in
    if i1 = i2 
    then (i1, g2)
    else addNode (BddNode (var, i1, i2)) g2


let buildBdd (formula : tformula) : bddGraph =
  snd @@ buildBdd' formula (VarsSet.elements @@ getVars formula) StringMap.empty emptyBddGraph


module IntMap = Map.Make(Int)

type reversedBddGraph = int * bddNode IntMap.t

let buildReversedBdd (formula : tformula) : reversedBddGraph =
  let (root, graph) = buildBdd formula in
  (root, (BddGraph.fold (fun node index acc -> IntMap.add index node acc) graph IntMap.empty))

let rec areReversedBddEquivalent ((root1, g1) : reversedBddGraph) ((root2, g2) : reversedBddGraph) : bool =
  match (IntMap.find root1 g1, IntMap.find root2 g2) with
  | (BddLeaf b1,BddLeaf b2) -> b1 == b2
  | (BddNode (name1, i1g1, i2g1), BddNode (name2, i1g2, i2g2)) ->
    if name1 <> name2 then
      false
    else
      areReversedBddEquivalent  (i1g1, g1) (i1g2, g2) 
      && areReversedBddEquivalent  (i2g1, g1) (i2g2, g2)
  | _ -> false



let isTautology (formula : tformula) : bool =
  match (BddGraph.bindings @@ snd @@ buildBdd formula) with
  | [(BddLeaf true, _)] -> true
  | _ -> false




let areEquivalent (f1 : tformula) (f2 : tformula) : bool =
  areReversedBddEquivalent (buildReversedBdd f1) (buildReversedBdd f2)


module Dot =
struct
  type style =
    | Red
    | Dashed
    | Bold

  type elt =
    | Node of (int*string)
    | Vertex of (int*int)

  type digraph = (elt * style list) list

  let empty = []

  let join = List.append

  let addWithStyle (elt : elt) (style : style list) : digraph -> digraph =
    List.cons (elt, style)

  let add (elt : elt) : digraph -> digraph =
    addWithStyle elt []

  let show_style = function
    | Red -> "color=red"
    | Dashed -> "style=dashed"
    | Bold -> "style=bold"

  let join_in_bracket (strings : string list) : string =
    match strings with
    | [] -> ""
    | _ -> " [ " ^ String.concat ", " strings ^ " ]"

  let show_elt ((elt, styles) : (elt * style list)) : string =
    let styles_str = List.map show_style styles in
    "   " ^
    (match elt with
     | Node(index, name) -> 
       string_of_int index ^ join_in_bracket ((("label=\"" ^ name ^ "\""))::styles_str)
     | Vertex (i1, i2) ->
       string_of_int i1 ^ " -> " ^ string_of_int i2 ^ join_in_bracket styles_str
    ) ^ ";\n"

  let show (name : string) (digraph : digraph) : string =
    "digraph " ^ name ^ " {\n" 
    ^ String.concat "" (List.map show_elt digraph)
    ^ "}"
end
(* 
let () =
    print_endline @@
         Dot.show "G" 
            [ Dot.Node (2, "Q2"), [] 
            ; Dot.Vertex  (2, 3), [ Dot.Bold; Dot.Red]
            ] *)

let bddNodeToDot (digraph : Dot.digraph) ((node, index) : (bddNode*int)) : Dot.digraph =
  match node with
  | BddNode (name, i1, i2) ->
    digraph
    |> Dot.add (Node (index, name))
    |> Dot.addWithStyle (Vertex (index, i1)) [Red; Dashed]
    |> Dot.add (Vertex (index, i2))
  | BddLeaf b ->
    digraph
    |> Dot.addWithStyle (Node (index, string_of_bool b)) [Bold]

let bddToDot ((_, graph) : bddGraph) : Dot.digraph =
  BddGraph.bindings graph
  |> List.fold_left bddNodeToDot Dot.empty



let rec decTreeToDot (index : int) (tree : decTree) : (int*Dot.digraph) =

  match tree with
  | DecLeaf b ->
    let newGraph =
      Dot.empty
      |> Dot.addWithStyle (Node (index, string_of_bool b)) [Bold]
    in
    (index + 1, newGraph)
  | DecRoot (name, left, right) ->
    let (i1, g1) = decTreeToDot (index) left in
    let (i2, g2) = decTreeToDot i1  right in

    ( i2 + 1
    , Dot.join g1 g2    
      |> Dot.add (Node (i2, name)) 
      |> Dot.addWithStyle (Vertex (i2, i1 -1)) [Red; Dashed]
      |> Dot.add (Vertex (i2, i2-1))
    )

