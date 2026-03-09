(**Path module signature *)

module type PATH = sig 
  type node
  type edge
  type weight

  (** For use the function to convert weight to string *)
  module Weight : sig
    val to_string : weight -> string
  end

  type step = {
    (** Source node of this step*)
    from : node;

    (** Destination node of this step*)
    to_node : node;

    (** The edge for this traversak*)
    edge : edge;

    (** The cost of this step proportional to the weight*)
    weight : weight;
  }

  type t = {
    (** List of all steps from source to destination*)
    steps : step list;
    total_weight : weight;
  }

  (** Print a path with nodes, lines and total weight *)
  val print_path : (node -> string) -> (edge -> string) -> (weight -> string) -> t -> unit
end 
