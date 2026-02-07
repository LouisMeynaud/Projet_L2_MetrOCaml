module type WEIGHT = sig
  type t
  (** The weight type (example, int for seconds, float for meters). *)

  type edge
  (** The type of edge data associated with this weight. *)

  val zero : t
  (** The identity element for addition (exemple 0 for time/distance). *)

  val infinity : t
  (** Value representing unreachable/infinite cost. *)

  val of_edge : edge -> t
  (** get the weight from edge data *)

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