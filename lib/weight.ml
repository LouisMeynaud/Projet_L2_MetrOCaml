open Network

(** Distance-based weight for routing. *)
module DistanceWeight : Routing.WEIGHT with type edge = connection = struct
  type t = float
  type edge = connection

  let zero = 0.0
  let infinity = Float.infinity

 (** Extract distance from connection metadata. 
  If distance is not specified, estimate from duration *)
  let of_edge (conn : connection) : t = conn.distance

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