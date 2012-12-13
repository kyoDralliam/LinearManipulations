val print_list :
  (Format.formatter -> 'a -> unit) ->
  ('b, 'c, 'd, 'e, 'e, 'b) format6 -> Format.formatter -> 'a list -> unit


val linear_type : Format.formatter -> LinearType.linear_type -> unit

val typed_lambda : Format.formatter -> SimplyTypedLambda.typed_lambda -> unit
