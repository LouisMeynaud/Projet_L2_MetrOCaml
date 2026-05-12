open Graph_intf
open Weight_intf

(** Module concret construisant un Itinéraire à partir d'un Graphe et d'un Poids
*)
module Make (Graph : GRAPH) (Weight : WEIGHT with type edge = Graph.Edge.t) =
struct
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
