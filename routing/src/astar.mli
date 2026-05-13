module Make
  (G : Graph.GRAPH)
  (W : Weight_intf.WEIGHT with type edge = G.Edge.t and type node = G.Node.t)
  (H : Heuristic_intf.HEURISTIC with type node = G.Node.t) :
  Algo_intf.ALGO
    with type graph = G.t
     and module P.Node = G.Node
     and module P.Edge = G.Edge
     and module P.Weight = W
