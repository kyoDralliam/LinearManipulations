open LinearType

type typed_lambda = typed_lambda_node Hashcons.hash_consed
and typed_lambda_node = 
    private
  | Var of string 
  | App of typed_lambda * typed_lambda list
  | Abs of (string * linear_type) list * typed_lambda
  | Des of string * string * typed_lambda * typed_lambda
  | Pair of typed_lambda * typed_lambda
  | Pro of typed_lambda
  | InjLeft of typed_lambda * linear_type * linear_type
  | InjRight of typed_lambda * linear_type * linear_type
  | Match of typed_lambda * (string * typed_lambda) * (string * typed_lambda)

      
val var : string -> typed_lambda
  
val app : typed_lambda -> typed_lambda -> typed_lambda
val apps : typed_lambda -> typed_lambda list -> typed_lambda
val abstr : string -> linear_type -> typed_lambda -> typed_lambda
val lam : (string * linear_type) list -> typed_lambda -> typed_lambda
  
val destr : string -> string -> typed_lambda -> typed_lambda -> typed_lambda
val pair : typed_lambda -> typed_lambda -> typed_lambda
  
val promote : typed_lambda -> typed_lambda

val left : typed_lambda -> linear_type -> linear_type -> typed_lambda
val right : typed_lambda -> linear_type -> linear_type -> typed_lambda
val match2 : typed_lambda -> (string * typed_lambda) -> (string * typed_lambda) -> typed_lambda
