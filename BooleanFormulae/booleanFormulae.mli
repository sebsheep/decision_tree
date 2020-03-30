open Types
include Types
val show_tformula : tformula -> string

module Parser: sig
  val parse : string -> (tformula, string) result
end


type decTree = DecLeaf of bool | DecRoot of string * decTree * decTree
module VarsSet :
sig
  type elt = String.t
  type t = Set.Make(String).t
  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val disjoint : t -> t -> bool
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val choose : t -> elt
  val choose_opt : t -> elt option
  val split : elt -> t -> t * bool * t
  val find : elt -> t -> elt
  val find_opt : elt -> t -> elt option
  val find_first : (elt -> bool) -> t -> elt
  val find_first_opt : (elt -> bool) -> t -> elt option
  val find_last : (elt -> bool) -> t -> elt
  val find_last_opt : (elt -> bool) -> t -> elt option
  val of_list : elt list -> t
  val to_seq_from : elt -> t -> elt Seq.t
  val to_seq : t -> elt Seq.t
  val add_seq : elt Seq.t -> t -> t
  val of_seq : elt Seq.t -> t
end

val getVars : tformula -> VarsSet.t
type env 
val empty_env : env
val env_of_bindings : (string * bool) list -> env
val evalFormula : env -> tformula -> bool
val buildDecTree : tformula -> decTree

type bddGraph

val buildBdd : tformula -> bddGraph
val isTautology : tformula -> bool
val areEquivalent : tformula -> tformula -> bool
module Dot :
sig

  type style = Red | Dashed | Bold
  type elt = Node of (int * string) | Vertex of (int * int)

  type digraph

  val empty : digraph
  val join : digraph -> digraph -> digraph
  val addWithStyle : elt -> style list -> digraph -> digraph
  val add : elt -> digraph -> digraph
  val join_in_bracket : string list -> string
  val show : string -> digraph -> string
end
val bddToDot : bddGraph -> Dot.digraph
val decTreeToDot : int -> decTree -> int * Dot.digraph
