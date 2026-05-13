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

module Make (N : NODE) (E : EDGE) : GRAPH with module Node = N and module Edge = E = struct
  module Node = N
  type node = N.t

  module Edge = E
  type edge = E.t

  type t = { adj : (node, (node * edge) list) Hashtbl.t }

  let create () = { adj = Hashtbl.create 1000 }
  let size g = Hashtbl.length g.adj
  let nodes g = Hashtbl.to_seq_keys g.adj |> List.of_seq
  let has_node g node = Hashtbl.mem g.adj node
  let add_node g node = if not (has_node g node) then Hashtbl.add g.adj node []

  let add_edge g src dst data =
    add_node g src;
    add_node g dst;
    let current = try Hashtbl.find g.adj src with Not_found -> [] in
    Hashtbl.replace g.adj src ((dst, data) :: current)

  let neighbors g node = try Hashtbl.find g.adj node with Not_found -> []
end
