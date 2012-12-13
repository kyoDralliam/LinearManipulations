

let (&) f x = f x

module SMap = Map.Make(struct type t = string let compare = Pervasives.compare end)

let list_from_string s = 
  let rec aux k acc = 
    if k < 0 then acc else aux (k-1) (s.[k] :: acc)
  in
  aux (String.length s - 1) []
