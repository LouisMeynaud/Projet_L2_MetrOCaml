open Path_intf
open Graph_intf
open Weight_intf
    
module Make (Graph : GRAPH) (W : WEIGHT with type edge = Graph.Edge.t) :
  PATH
    with module Node = Graph.Node
      and module Edge = Graph.Edge
      and module Weight = W = struct
  module Node = Graph.Node
    
  type node = Node.t
    
  module Edge = Graph.Edge
    
  type edge = Edge.t
    
  module Weight = W
    
  type weight = Weight.t
  type step_data = Weight.step_data
  type step = { from : node; to_node : node; edge : edge; weight : weight; data : step_data }
  type t = { steps : step list; total_weight : weight }
end