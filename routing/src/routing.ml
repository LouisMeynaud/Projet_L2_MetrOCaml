(** Routing library - Public API. This module re-exports all public interfaces
    and functors for use by applications. Import this module to access the
    routing library. *)

module type NODE = Graph_intf.NODE
module type EDGE = Graph_intf.EDGE
module type GRAPH = Graph_intf.GRAPH

module Graph = Graph

module type PATH = Path_intf.PATH
module type WEIGHT = Weight_intf.WEIGHT
module type ALGO = Algo_intf.ALGO
module type HEURISTIC = Heuristic_intf.HEURISTIC

module Path = Path
module Combinator = Combinator
module Bench = Bench
module Bfs = Bfs
module Dijkstra = Dijkstra
module Astar = Astar
