(** Path module

    Generics paths that represent routes through any graph with any weight
    system **)

open Path_intf
open Graph_intf

module Make
    (Graph : GRAPH)
    (Weight : Weight_intf.WEIGHT with type edge = Graph.Edge.t) :
  PATH
    with type node = Graph.Node.t
     and type edge = Graph.Edge.t
     and type weight = Weight.t = struct
  type node = Graph.Node.t
  type edge = Graph.Edge.t
  type weight = Weight.t
  type step = { from : node; to_node : node; edge : edge; weight : weight }
  type t = { steps : step list; total_weight : weight }
end
