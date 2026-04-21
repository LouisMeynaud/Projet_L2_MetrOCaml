open Network

module Forceheight : Routing.WEIGHT with type edge = connection and type node = person = struct
  type t = float
  type edge = connection
  type node = person
  type step_data = unit
  type state_key = unit

  let start_at _time = 0.0
  let infinity = Float.infinity
  let is_inf w = w = Float.infinity

  let traverse current_weight (_ : node) edge =
    if edge.force <= 0.0 then 
      None
    else
      let cost = 1.0 /. edge.force in
      Some (current_weight +. cost, ())

  let compare a b =
    if a = b then 0.0
    else if a < b then -1.0
    else 1.0

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0

  let get_time _w = 0
  
  let to_string w = 
    if is_inf w then "∞" 
    else Printf.sprintf "Coût : %.2f" w

  let step_data_to_string () = ""
  let get_state_key _w = ()
end