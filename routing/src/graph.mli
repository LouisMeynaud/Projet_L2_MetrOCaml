module type NODE = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
end

module type EDGE = sig
  type t
end

module type GRAPH = sig
  module Node : NODE
  type node = Node.t

  module Edge : EDGE
  type edge = Edge.t

  type t

  val create : unit -> t
  val nodes : t -> node list
  val has_node : t -> node -> bool
  val add_node : t -> node -> unit
  val add_edge : t -> node -> node -> edge -> unit
  val size : t -> int
  val neighbors : t -> node -> (node * edge) list
end

module Make (N : NODE) (E : EDGE) : GRAPH with module Node = N and module Edge = E
