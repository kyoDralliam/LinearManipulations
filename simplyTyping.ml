open Tools
open LinearType
open SimplyTypedLambda
open Hashcons

exception OutOfContext of string
exception NotWellTyped of string * (typed_lambda * linear_type) list
exception NotLinear of typed_lambda * linear_type

let rec get_linear_part t = 
  match t.node with
    | OfCourse t -> get_linear_part t
    | t' -> t

let rec is_contraction t1 t2 = 
  t1 == t2 ||
  match t2.node with 
    | OfCourse t2' -> is_contraction t1 t2'
    | _ -> false


let map_diff m1 m2 =
  SMap.fold (fun k v m -> SMap.remove k m) m2 m1

let rec type_of_term_aux ctx t =
  match t.node with
  | Var x -> 
      begin 
        try let typ = SMap.find x ctx in
            typ, 
            begin match typ.node with 
              | OfCourse _ -> ctx
              | _ -> SMap.remove x ctx
            end
        with Not_found -> raise (OutOfContext x)
      end

  | App (t, args) ->
      let (ctx', types0) = 
        let fold_fun (ctx, acc) t0 = 
          let typ, ctx' = type_of_term_aux ctx t0 in 
          ctx', typ :: acc 
        in
        List.fold_left fold_fun (ctx, []) (t::args)
      in
      let (typ_t, types) =
        match List.rev types0 with 
          | typ_t :: types -> (typ_t, types)
          | _ -> assert false
      in
      let rec cut (largs, t1) (arg, t2) = 
        match t1.node with 
          | OfCourse t1' -> cut (largs, t1') (arg, t2)
          | Arr (t1s, t1b) when is_contraction t1s t2 -> (arg :: largs, t1b)
          | Arr _ -> 
              let t0 = apps t largs in 
              raise (NotWellTyped ("Argument of wrong type", [t0, t1; arg, t2]))
          | _ -> 
              let t0 = apps t largs in 
              raise (NotWellTyped ("Should have an arrow type", [t0, t1; arg, t2]))
      in
      snd (List.fold_left cut ([],typ_t) (List.combine args types)), ctx'

  | Abs (args, t) -> 
      let aux () =
        let typ_t, ctx' = 
          let ctx = 
            List.fold_left (fun ctx (x, typ_x) -> SMap.add x typ_x ctx) ctx args 
          in type_of_term_aux ctx t in
        let add_arg_type (x,typ) typ0 = 
          match typ.node with 
            | OfCourse _ -> typ => typ0
            | _ -> 
                if SMap.mem x ctx'
                then raise (NotLinear (var x, typ))
                else typ => typ0
        in 
        let typ = List.fold_right add_arg_type args typ_t in
      
        typ, ctx'
      in

      variable_shadowing (List.map fst args) ctx aux

  | Des (x1,x2,x,t) -> 
      let typ_x, ctx' = type_of_term_aux ctx x in
      let typ_x1, typ_x2 = 
        match (get_linear_part typ_x).node with 
          | Times (typ_x1, typ_x2) -> typ_x1, typ_x2
          | _ -> 
              raise (NotWellTyped ("Should have pair type", [x, typ_x]))
      in
      
      let aux () =
        let ctx'' = SMap.add x1 typ_x1 (SMap.add x2 typ_x2 ctx') in
        type_of_term_aux ctx'' t 
      in

      variable_shadowing [x1 ; x2] ctx' aux

  | Pair (t1, t2) -> 
      let typ_t1, ctx' = type_of_term_aux ctx t1 in
      let typ_t2, ctx'' = type_of_term_aux ctx' t2 in
      times typ_t1 typ_t2, ctx'' 

  | Pro t ->
      let typ_t, ctx' = type_of_term_aux ctx t in
      let env = map_diff ctx ctx' in
      if SMap.for_all (fun _ t -> match t.node with OfCourse _ -> true | _ -> false) env
      then !typ_t, ctx'
      else failwith "cannot promote this term"

  | InjLeft (t, typ1, typ2) ->
      let typ_t, ctx' = type_of_term_aux ctx t in
      if typ_t == typ1 
      then plus typ1 typ2, ctx'
      else failwith "bad type annotation in sum construct"

  | InjRight (t, typ1, typ2) ->
      let typ_t, ctx' = type_of_term_aux ctx t in
      if typ_t == typ2
      then plus typ1 typ2, ctx'
      else failwith "bad type annotation in sum construct"

  | Match (t, (x1, t1), (x2, t2)) ->
      let typ_t, ctx' = type_of_term_aux ctx t in
      let typ1, typ2 = 
        match (get_linear_part typ_t).node with 
          | Plus (typ1, typ2) -> (typ1, typ2)
          | _ -> raise (NotWellTyped ("Should have plus type", [t, typ_t]))
      in 

      let aux () = 
        let ctx' = SMap.add x1 typ1 (SMap.add x2 typ2 ctx') in
        let typ_t1, ctx_1 = type_of_term_aux ctx' t1 in
        let typ_t2, ctx_2 = type_of_term_aux ctx' t2 in
        if typ_t1 == typ_t2 && SMap.equal (==) ctx_1 ctx_2
        then typ_t1, ctx_1
        else 
          let msg = "Each branch of the match should have the same type and use the same context" in
          raise (NotWellTyped (msg, [t1, typ_t1 ; t2, typ_t2]))
      in
        
      variable_shadowing [ x1 ; x2 ] ctx' aux

and variable_shadowing var_list ctx f = 
  let has_old_binding = List.filter (fun x -> SMap.mem x ctx) var_list in
  let old_binding = List.map (fun x -> (x,SMap.find x ctx)) has_old_binding in
  
  let typ, ctx' = f () in

  let ctx'' = List.fold_left (fun acc (x,y) -> SMap.add x y acc) ctx' old_binding in

  typ, ctx''

let type_of_term ctx term = 
  let typ, ctx' = type_of_term_aux ctx term in
  if SMap.for_all (fun _ t -> match t.node with OfCourse _ -> true | _ -> false) ctx'
  then typ
  else failwith "this derivation is not linear"
