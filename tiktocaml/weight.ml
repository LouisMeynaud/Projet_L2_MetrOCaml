open Network

module ForceWeight : Routing.WEIGHT with type edge = connection and type node = person = struct
  type t = float
  type edge = connection
  type node = person
  type state_key = unit

  let start_at _time = 0.0
  let infinity = Float.infinity
  let is_inf = Float.is_infinite

  let traverse current_weight (_ : node) edge =
    if edge.force <= 0.0 then None
    else Some (current_weight +. (1.0 /. edge.force))

  let compare a b = float_of_int (Float.compare a b)
  let ( < ) a b = Float.compare a b < 0
  let ( > ) a b = Float.compare a b > 0
  let ( = ) a b = Float.compare a b = 0

  let get_weight _w = 0

  let to_string w =
    if Float.is_infinite w then "∞"
    else Printf.sprintf "Coût : %.2f" w

  let get_state_key _w = ()
end
