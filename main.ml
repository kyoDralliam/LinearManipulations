open LinearType
open SimplyTypedLambda
open SimplyTyping


open Tools

let [ ta ; tb ; tc ; td ] = List.map (fun x -> base (Char.escaped x)) & list_from_string "ABCD"
let [ x ; y ; z ; u ; v ; w ; f ; g ] = 
  List.map (fun x -> var (Char.escaped x)) & list_from_string "xyzuvwfg"

let zero = lam ["x", ta ; "f", !(ta => ta)] x

let _ = Format.printf "type of zero zero : %a@\n" 
  Print.linear_type (type_of_term SMap.empty zero)
let _ = Format.printf "zero :%a@\n" Print.typed_lambda zero

let nat = ta => (!(ta => ta) => ta)

let succ = 
  let f = var "f" in 
  let n = var "n" in
  abstr "n" nat (abstr "x" ta (abstr "f" !(ta => ta) (app (app n (app f x)) f)))

let _ = Format.printf "type nat : %a@\n" Print.linear_type nat 
let _ = Format.printf "succ : %a@\n" Print.typed_lambda succ
let _ = Format.printf "type of succ : %a@\n" 
  Print.linear_type (type_of_term SMap.empty succ)

let one = app succ zero

let _ = Format.printf "one : %a@\n" Print.typed_lambda one
let _ = Format.printf "type of one : %a@\n" Print.linear_type (type_of_term SMap.empty one)


let curry = 
  let f = var "f" in
  lam ["f", (times ta tb) => tc ; "x", ta ; "y", tb]
    (app f (pair x y))

let _ = Format.printf "curry : %a@\n" Print.typed_lambda curry
let _ = Format.printf "type of curry : %a@\n" Print.linear_type (type_of_term SMap.empty curry)




let uncurry = 
  let f = var "f" in
  lam ["f", ta => (tb => tc); "z", times ta tb] 
    (destr "x" "y" z (apps f [x ; y]))

let _ = Format.printf "uncurry : %a@\n" Print.typed_lambda uncurry
let _ = Format.printf "type of uncurry : %a@\n" 
  Print.linear_type (type_of_term SMap.empty uncurry)



let promotion_test = 
  Format.printf "@\nTest of promotion :@\n" ;

  let modus_ponens =
    lam ["x", !ta ; "u", !(ta => tb)] (promote (app u x)) in

  Format.printf "modus ponens : %a@\n" Print.typed_lambda modus_ponens ;
  Format.printf "type of modus ponens : %a@\n" 
    Print.linear_type (type_of_term SMap.empty modus_ponens) ;
  true


let test_nat = 
  let cin = open_in "test_nat" in
  let terms, types = Parser.file Lexer.token (Lexing.from_channel cin) in

  Format.printf "@\ntest_nat : @\n" ;

  let print_results (s, t) = 
    Format.printf "term %s : %a@\n" s Print.typed_lambda t ;
    Format.printf "type of %s : %a@\n" s Print.linear_type 
      (SimplyTyping.type_of_term Tools.SMap.empty t) 
  in

  List.iter print_results terms ;
  
  true


let test_plus = 
  let l = base "left" in 
  let r = base "right" in 
  let r_fun = lam [ "x" , r ] (right x l r) in
  let codiag = lam [ "x" , plus ta ta] (match2 x ("y", y) ("y", y)) in
  let t = pair (r_fun) (codiag) in
  Format.printf "%a@\n" Print.typed_lambda t ;
  Format.printf "%a@\n" Print.linear_type (SimplyTyping.type_of_term Tools.SMap.empty t)
    
