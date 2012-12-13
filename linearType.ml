open Tools

type linear_type = linear_type_node Hashcons.hash_consed
and  linear_type_node = 
  | Base of string
  | Times of linear_type * linear_type
  | Plus of linear_type * linear_type
  | OfCourse of linear_type 
  | Arr of linear_type * linear_type
  (*   
       | With of linear_type * linear_type
       | Par of linear_type * linear_type 
       | Not of linear_type 
       | WhyNot of linear_type 
  *)

module LinearTypeNode =
struct 
  type t = linear_type_node
  let equal t1 t2 = 
    match (t1, t2) with 
      | Base s1, Base s2 -> s1 = s2
      | Times (ta1, ta2), Times (tb1, tb2) 
      | With (ta1, ta2), With (tb1, tb2) 
      | Plus (ta1, ta2), Plus (tb1, tb2) 
      | Arr (ta1, ta2), Arr (tb1, tb2) ->
          ta1 == tb1 && ta2 == tb2
      | OfCourse t1, OfCourse t2 ->
            t1 == t2
      | _ -> false
          
  let f x y = 19 * x + y
    
  let hash = function 
    | Base s -> Hashtbl.hash s
    | Times (ta1, ta2) -> abs (f (f ta1.hkey ta2.hkey) 1)
    | With (ta1, ta2) -> abs (f (f ta1.hkey ta2.hkey) 2)
    | Plus (ta1, ta2) -> abs (f (f ta1.hkey ta2.hkey) 3)
    | Arr (ta1, ta2) -> abs (f (f ta1.hkey ta2.hkey) 4)
    | OfCourse t -> abs (f t.hkey 5)
end
  
module HType = Hashcons.Make(LinearTypeNode)
  
let table = HType.create 171
  
let base s = HType.hashcons table & Base s
let times t1 t2 = HType.hashcons table & Times (t1, t2)
let (!) t = HType.hashcons table & OfCourse t
let (=>) t1 t2 = HType.hashcons table & Arr (t1, t2)
