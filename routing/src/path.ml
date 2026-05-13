open Graph
open Weight_intf

module type PATH = sig
  module Node : NODE
  type node = Node.t

  module Edge : EDGE
  type edge = Edge.t

  module Weight : WEIGHT
  type weight = Weight.t

  type step = {
    from : node;
    to_node : node;
    edge : edge;
    weight : weight;
  }

  type t = { steps : step list; total_weight : weight }
end

module Make (Graph : GRAPH) (Weight : WEIGHT with type edge = Graph.Edge.t) = struct
  module Node = Graph.Node
  module Edge = Graph.Edge
  module Weight = Weight

  type node = Node.t
  type edge = Edge.t
  type weight = Weight.t
  type step = {
    from : node;
    to_node : node;
    edge : edge;
    weight : weight;
  }

  type t = { steps : step list; total_weight : weight }
end
