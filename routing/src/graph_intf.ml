(** Generic graph interface for routing algorithms. This module defines the
    abstract interface for graphs, nodes, and edge data. It is completely
    domain-agnostic and can be used for any graph-based routing. *)

(** Node data abstraction. Node data is opaque to the graph structure. *)
module type NODE = sig
  type t
  (** The node type. *)

  val equal : t -> t -> bool
  (** Equality comparison for nodes. *)

  val hash : t -> int
  (** Hash function for use in hash tables. *)
end

(** Edge data abstraction. Edge data is opaque to the graph structure. *)
module type EDGE = sig
  type t
  (** The edge type. *)
end

(** Graph abstraction. Represents a directed graph with typed nodes and edges.
    Parameterised by node type and edge type. *)
module type GRAPH = sig
  module Node : NODE

  type node = Node.t

  module Edge : EDGE

  type edge = Edge.t

  type t
  (** The graph type. *)

  val create : unit -> t
  (** Create an empty graph. *)

  val nodes : t -> node list
  (** [nodes g] returns all nodes in the graph. *)

  val has_node : t -> node -> bool
  (** [has_node g n] checks if node [n] exists in the graph. *)

  val add_node : t -> node -> unit
  (** [add_node g n] adds a node [n] to the graph. *)

  val add_edge : t -> node -> node -> edge -> unit
  (** [add_edge g src dst data] adds a directed edge from [src] to [dst] with
      associated edge data. *)

  val size : t -> int
  (** [size g] returns the number of nodes in [g] in O(1). *)

  val neighbors : t -> node -> (node * edge) list
  (** [neighbors g n] returns all nodes reachable from [n] along with the edge
      data for each edge. *)
end
