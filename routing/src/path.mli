module type PATH = sig
  module Node : Graph.NODE
  type node = Node.t

  module Edge : Graph.EDGE
  type edge = Edge.t

  module Weight : Weight_intf.WEIGHT
  type weight = Weight.t

  type step = {
    from : node;
    to_node : node;
    edge : edge;
    weight : weight;
  }

  type t = { steps : step list; total_weight : weight }
end

module Make
  (G : Graph.GRAPH)
  (Weight : Weight_intf.WEIGHT with type edge = G.Edge.t) :
  PATH
    with module Node = G.Node
     and module Edge = G.Edge
     and module Weight = Weight
