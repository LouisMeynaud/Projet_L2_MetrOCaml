open Graph_intf

module Make (N : NODE) (E : EDGE) :
  GRAPH with module Node = N and module Edge = E = struct
  module Node = N

  type node = N.t

  module Edge = E

  type edge = E.t
  type t = { adj : (node, (node * edge) list) Hashtbl.t }

  let create () = { adj = Hashtbl.create 1000 }
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
