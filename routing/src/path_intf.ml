open Graph_intf
open Weight_intf

module type PATH = sig
  module Node : NODE

  type node = Node.t

  module Edge : EDGE

  type edge = Edge.t

  module Weight : WEIGHT

  type weight = Weight.t

  type step_data = Weight.step_data
  (** The opaque payload generated during edge traversal *)

  type step = {
    from : node;
    to_node : node;
    edge : edge;
    weight : weight;
    data : step_data;
  }
  (** A single step in a path, representing a traversal from one node to another
      containing a source, destination edge, weight, and step data *)

  type t = { steps : step list; total_weight : weight }
  (** List of all steps from source to destination*)
end
