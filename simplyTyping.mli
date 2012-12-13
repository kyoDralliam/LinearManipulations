open LinearType
open SimplyTypedLambda

exception OutOfContext of string

exception NotWellTyped of string *
            (typed_lambda * linear_type) list
exception NotLinear of typed_lambda *
            linear_type

val get_linear_part : linear_type -> linear_type

val is_contraction : linear_type -> linear_type -> bool

val type_of_term : linear_type Tools.SMap.t -> typed_lambda -> linear_type
