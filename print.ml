open Format
open Tools

let print_list f sep ppf = function 
  | [] -> fprintf ppf "" 
  | [x] -> fprintf ppf "%a" f x
  | x :: xs -> 
      let g ppf = List.iter (fprintf ppf "%(%)%a" sep f) in
      fprintf ppf "%a%a" f x g xs
        


open Hashcons
open LinearType

type side = L | R | N
let cond lvl lvl' side = lvl > lvl' || (lvl = lvl' && side = L) 
  
let rec linear_type_aux lvl side ppf t =
  match t.node with
    | Base s -> pp_print_string ppf s
    | Times (t1,t2) -> 
        let f = linear_type_aux 4 in
        fprintf ppf (if cond lvl 4 side then "(%a * %a)" else "%a * %a") (f L) t1 (f R) t2
    | Plus (t1, t2) -> 
        let f = linear_type_aux 2 in 
        fprintf ppf (if cond lvl 2 side then "(%a + %a)" else "%a + %a") (f L) t1 (f R) t2
(*    | With (t1, t2) -> 
        let f = linear_type_aux 2 in 
        fprintf ppf (if cond lvl 2 side then "(%a & %a)" else "%a & %a") (f L) t1 (f R) t2 *)
    | OfCourse t -> fprintf ppf "!%a" (linear_type_aux 5 N) t
    | Arr (t1, t2) -> 
        let f = linear_type_aux 4 in 
        fprintf ppf (if cond lvl 4 side then "(%a --o %a)" else "%a --o %a") (f L) t1 (f R) t2


let linear_type = linear_type_aux 0 N


open SimplyTypedLambda


let rec typed_lambda_aux b ppf t =
  match t.node with
    | Var x -> pp_print_string ppf x
    | App (t, args) -> 
        fprintf ppf (if b then "(%a %a)" else "%a %a") 
          (typed_lambda_aux true) t (print_list (typed_lambda_aux true) " ") args
    | Abs (args, t) -> 
        let g ppf (x, typ) = fprintf ppf "%s:%a" x linear_type typ in
        fprintf ppf (if b then "(\\%a.%a)" else "\\%a.%a") 
          (print_list g " ") args (typed_lambda_aux false) t
    | Des (x1, x2, x, t) ->
        fprintf ppf "destruct %a as (%s, %s) in %a" 
          (typed_lambda_aux false) x x1 x2 (typed_lambda_aux false) t
    | Pair (t1, t2) ->
        fprintf ppf "< %a ; %a >" (typed_lambda_aux false) t1 (typed_lambda_aux false) t2
    | Pro t ->
        fprintf ppf "!%a" (typed_lambda_aux true) t

let typed_lambda = typed_lambda_aux false