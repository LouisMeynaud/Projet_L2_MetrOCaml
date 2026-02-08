open Network


module TimeWeight : Routing.Weight_intf.WEIGHT with type edge = connection = struct
  type t = int
  type edge = connection

  let zero = 0
  let infinity = max_int

  let of_edge (conn : connection) = conn.duration

  let add = ( + )

  let compare = Int.compare

  let ( < ) a b = compare a b < 0
  let ( > ) a b = compare a b > 0
  let ( = ) a b = compare a b = 0

  let to_string w =
    if w = infinity then "∞"
    else
      let hours = w / 3600 in
      let minutes = (w mod 3600) / 60 in
      let seconds = w mod 60 in
      if hours > 0 then Printf.sprintf "%dh%02dm%02ds" hours minutes seconds
      else if minutes > 0 then Printf.sprintf "%dm%02ds" minutes seconds
      else Printf.sprintf "%ds" seconds
end


(** Distance-based weight for routing. *)
module DistanceWeight : Routing.Weight_intf.WEIGHT with type edge = connection = struct
  type t = float
  type edge = connection

  let zero = 0.0
  let infinity = Float.infinity

 (** Extract distance from connection metadata. 
  If distance is not specified, estimate from duration *)
  let of_edge (conn : connection) : t =
    match conn.distance with
    | Some d -> d
    | None -> float_of_int conn.duration *. 10.0  (* ~10 m/s avg speed *)


  let add = ( +. )

  let compare = Float.compare

  let ( < ) a b = compare a b < 0
  let ( > ) a b = compare a b > 0
  let ( = ) a b = compare a b = 0

  let to_string d =
    if d = infinity then "∞"
    else if d >= 1000.0 then Printf.sprintf "%.1f km" (d /. 1000.0)
    else Printf.sprintf "%.0f m" d
end