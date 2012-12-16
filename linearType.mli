
type linear_type = linear_type_node Hashcons.hash_consed
and  linear_type_node = 
    private
  | Base of string
  | TypeVar of string
  | Times of linear_type * linear_type
  | Plus of linear_type * linear_type
  | OfCourse of linear_type 
  | Arr of linear_type * linear_type
  | Forall of string * linear_type 
  | TypeAbs of string * linear_type
(* 
   | With of linear_type * linear_type
   | Par of linear_type * linear_type 
   | Not of linear_type 
   | WhyNot of linear_type 
*)

val base : string -> linear_type
val tvar : string -> linear_type
val plus : linear_type -> linear_type -> linear_type
val times : linear_type -> linear_type -> linear_type
val (!) : linear_type -> linear_type
val (=>) : linear_type -> linear_type -> linear_type
val forall : string -> linear_type -> linear_type
val (=>>) : string -> linear_type -> linear_type
