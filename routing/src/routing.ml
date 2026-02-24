(** Routing library - Public API. This module re-exports all public interfaces
    and functors for use by applications. Import this module to access the
    routing library. *)

module type NODE = Graph_intf.NODE
module type EDGE = Graph_intf.EDGE
module type GRAPH = Graph_intf.GRAPH

module Graph = Graph

module type WEIGHT = Weight_intf.WEIGHT

module Bfs = Bfs
module Dijkstra = Dijkstra
