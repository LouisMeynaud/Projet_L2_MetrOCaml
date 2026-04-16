(** Abstract weight interface for time-dependent routing algorithms. *)
module type WEIGHT = sig
  type t
  type edge

  type node

  type step_data
  (** Opaque payload generated during edge traversal (e.g., wait times). *)

  type state_key
  (** NEW: Opaque key used to differentiate states at the same node (State-Space
      Routing). *)

  val start_at : int -> t
  (** Initialize the weight with a departure time (in seconds from midnight). *)

  val infinity : t
  (** Value representing unreachable/infinite cost. *)

  val is_inf : t -> bool
  (** Check if the weight is infinity. *)

  val traverse : t -> node -> edge -> (t * step_data) option
  (** Attempt to traverse an edge. Returns None if impossible (e.g. no train),
      otherwise returns the new weight and the opaque step_data. *)

  val compare : t -> t -> float
  (** Normalised comparison returning a value within [-1, 1]. *)

  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val get_time : t -> int
  val to_string : t -> string

  val step_data_to_string : step_data -> string
  (** Formats the opaque step_data into a printable string for UI/Show. *)

  val get_state_key : t -> state_key
  (** NEW: Extracts the state key from the current weight to prevent Dijkstra
      from prematurely pruning paths that have different context (e.g. different
      subway lines). *)
end
