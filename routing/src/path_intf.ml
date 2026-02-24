(**Path module signature *)

module type PATH = sig 
  type node
  type edge
  type weight

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

  (** Print a path using a function to print nodes and weights.*)
  val print_path : (node -> string) -> (weight -> string) -> t -> unit
end 
