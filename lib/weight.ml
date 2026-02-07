open Network

(** Distance-based weight for routing. *)
module DistanceWeight : Routing.Weight_intf.weight with type edge = connection = struct
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