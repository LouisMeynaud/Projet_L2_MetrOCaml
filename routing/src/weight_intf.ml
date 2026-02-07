module type weight_intf = sig
  type t
  (** The weight type (example, int for seconds, float for meters). *)
  val infinity : t
  (** Value representing unreachable/infinite cost. *)

  val add : t -> t -> t
  (** Combine two weights (must handle infinity correctly). *)
  val compare : t -> t -> int
  (** Total ordering for priority queue operations.
      Returns negative if first < second, 0 if equal, positive if first > second. *)

  val ( < ) : t -> t -> bool
  (** Strict less-than for convenience. *)

  val ( > ) : t -> t -> bool
  (** Strict greater-than for convenience. *)

  val ( = ) : t -> t -> bool
  (** Equality for convenience. *)

  val to_string : t -> string
  (** Convert weight to string for debugging/logging. *)

end