(**Path module signature *)

open Graph_intf
open Weight_intf

module type PATH = sig 
  module Node : NODE
  type node = Node.t

  module Edge : EDGE
  type edge = Edge.t

  module Weight : WEIGHT
  type weight = Weight.t

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
end 
