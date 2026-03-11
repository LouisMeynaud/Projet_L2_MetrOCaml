(** Abstract weight interface for routing algorithms. Weight modules are
    responsible for interpreting edge data and producing routing costs. The
    routing algorithms operate on these abstract weights. *)

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

  val is_inf : t -> bool
  (** Check if the weight is infinity. *)

  val add : t -> t -> t
  (** Combine two weights (must handle infinity correctly). *)

  val compare : t -> t -> float
  (** Normalised comparison returning a value within [-1, 1] representing the magnitude of the difference.
      Negative means first < second,
      Positive means first > second,
      Zero means first = second.
      Magnitude reflects how much better/worse the first is compared to the second,
      Must handle infinity correctly. *)

  val ( < ) : t -> t -> bool
  (** Strict less-than.  Equivalent to [compare a b < 0.0]. *)

  val ( > ) : t -> t -> bool
  (** Strict greater-than.  Equivalent to [compare a b > 0.0]. *)

  val ( = ) : t -> t -> bool
  (** Equality.  Equivalent to [compare a b = 0.0]. *)

  val to_string : t -> string
  (** Convert weight to string for debugging/logging. *)
end
