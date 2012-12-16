open Tools
open LinearType
open Hashcons

type typed_lambda = typed_lambda_node Hashcons.hash_consed
and typed_lambda_node =
  | Var of string 
  | App of typed_lambda * typed_lambda list
  | Abs of (string * linear_type) list * typed_lambda
  | Des of string * string * typed_lambda * typed_lambda
  | Pair of typed_lambda * typed_lambda
  | Pro of typed_lambda
  | InjLeft of typed_lambda * linear_type * linear_type
  | InjRight of typed_lambda * linear_type * linear_type
  | Match of typed_lambda * (string * typed_lambda) * (string * typed_lambda)
      
module TypedLambdaNode =
struct
  type t = typed_lambda_node
  let equal t1 t2 = 
    match (t1, t2) with 
      | Var x1, Var x2 -> x1 = x2
      | App (t1, tl1), App (t2, tl2) -> 
          t1 == t2 && tl1 == tl2
      | Abs (args1, t1), Abs (args2, t2) ->
          t1 == t2 && List.for_all2 (fun (x1,y1) (x2,y2) -> x1 = x2 && y1 == y2) args1 args2
      | Des (x1a, x2a, xa, ta), Des (x1b, x2b, xb, tb) ->
          x1a = x1b && x2a = x2b && xa == xb && ta == tb
      | Pair (t1a, t2a), Pair (t1b, t2b) ->
          t1a == t1b && t2a == t2b
      | Pro t1, Pro t2 -> t1 == t2
      | InjLeft (ta, typa1, typa2), InjLeft (tb, typb1, typb2) 
      | InjRight (ta, typa1, typa2), InjRight (tb, typb1, typb2) ->
          ta == tb && typa1 == typb1 && typa2 == typb2 
      | Match (ta, (xa1, ta1), (xa2, ta2)), Match (tb, (xb1, tb1), (xb2, tb2)) ->
          ta == tb && xa1 = xb1 && ta1 == tb1 && xa2 = xb2 && ta2 == tb2
      | _ -> false
          
  let f x y = 19 * x + y

  let hash = function
    | Var x -> Hashtbl.hash x
    | App (t, tl) -> abs (f (List.fold_left f t.hkey (List.map (fun x -> x.hkey) tl)) 1)
    | Abs (args, t) -> 
        let g acc (x,y) = f (f acc & Hashtbl.hash x) y.hkey in
        abs (f (List.fold_left g t.hkey args) 2)
    | Des (x1, x2, x, t) ->
        abs (f (f (f (f (Hashtbl.hash x1) (Hashtbl.hash x2)) x.hkey) t.hkey) 3)
    | Pair (t1, t2) ->
        abs (f (f t1.hkey t2.hkey) 4)
    | Pro t -> abs (f t.hkey 5)
    | InjLeft (ta, typa1, typa2) ->
        abs (f (f (f ta.hkey typa1.hkey) typa2.hkey) 6)
    | InjRight (ta, typa1, typa2) ->
        abs (f (f (f ta.hkey typa1.hkey) typa2.hkey) 7)
    | Match (ta, (xa1, ta1), (xa2, ta2)) ->
        abs (f (f (f (f ta.hkey & Hashtbl.hash xa1) ta1.hkey) & Hashtbl.hash xa2) ta2.hkey) 

end
  
module HTypedLambda = Hashcons.Make(TypedLambdaNode)
  
let table = HTypedLambda.create 171
  
let var x = HTypedLambda.hashcons table & Var x
  
let apps t1 tl = 
  let n = 
    match t1.node with 
      | App (t, tl') -> App (t, tl' @ tl)
      | _ -> App(t1, tl)
  in
  HTypedLambda.hashcons table n
    
let app t1 t2 = apps t1 [t2]
  
let lam args t = 
  let n = 
    match t.node with 
      | Abs (args', t') -> Abs (args @ args', t')
      | _ -> Abs (args, t)
  in 
  HTypedLambda.hashcons table n
    
let abstr x typ t = lam [x, typ] t
  
let destr x1 x2 x t = HTypedLambda.hashcons table & Des (x1,x2,x,t)
  
let pair t1 t2 = HTypedLambda.hashcons table & Pair (t1, t2)

let promote t = HTypedLambda.hashcons table & Pro t

let left t typ1 typ2 = 
  HTypedLambda.hashcons table &  InjLeft (t, typ1, typ2)

let right t typ1 typ2 = 
  HTypedLambda.hashcons table & InjRight (t, typ1, typ2)

let match2 t branch1 branch2 = 
  HTypedLambda.hashcons table & Match (t, branch1, branch2)
