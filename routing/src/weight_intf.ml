(** Abstract weight interface for time-dependent routing algorithms. *)
module type WEIGHT = sig
  type t
  type edge

  val start_at : int -> t
  (** Initialize the weight with a departure time (in seconds from midnight). *)

  val infinity : t
  (** Value representing unreachable/infinite cost. *)

  val is_inf : t -> bool
  (** Check if the weight is infinity. *)

  val add : t -> edge -> t
  (** Combine the accumulated weight at a node with the next edge. 
      This function checks schedules based on the current time. *)

  val compare : t -> t -> float
  (** Normalised comparison returning a value within [-1, 1]. *)

  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( = ) : t -> t -> bool

  val to_string : t -> string
end