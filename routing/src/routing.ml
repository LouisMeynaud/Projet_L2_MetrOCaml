(** Bibliothèque de routage : API publique. Ce module réexporte toutes les interfaces publiques
    et foncteurs pour être utilisés par les applications. Importez seulement ce module pour accéder à la
    bibliothèque de routage. *)

module type NODE = Graph.NODE
module type EDGE = Graph.EDGE
module type GRAPH = Graph.GRAPH
module type PATH = Path.PATH
module type WEIGHT = Weight_intf.WEIGHT
module type ALGO = Algo_intf.ALGO
module type HEURISTIC = Heuristic_intf.HEURISTIC

module Graph = Graph
module Path = Path
module Combinator = Combinator
module Bench = Bench
module Bfs = Bfs
module Dijkstra = Dijkstra
module Astar = Astar
