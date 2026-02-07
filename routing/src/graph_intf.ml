(** Generic graph interface for routing algorithms.
  This module defines the abstract interface for graphs, nodes, and edge data.
  It is completely domain-agnostic and can be used for any graph-based routing.
*)

(** Node data abstraction.
  Node data is opaque to the graph structure. *)
module type NODE = sig
  type t
  (** The node type. *)
end

(** Edge data abstraction.
  Edge data is opaque to the graph structure. *)
module type EDGE = sig
  type t
  (** The edge type. *)
end

(** Graph abstraction.
  Represents a directed graph with typed nodes and edges.
  Parameterised by node type and edge type. *)
module type GRAPH = sig
  module Node : NODE
  type node = Node.t

  module Edge : EDGE
  type edge = Edge.t
  
  type t
  (** The graph type. *)
end
