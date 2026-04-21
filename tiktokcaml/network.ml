type person = {
  name : string;
  age : int;
}

type connection = {
  force : float;
}

module StationNode : Routing.NODE with type t = person = struct
  type t = person

  let equal a b = String.equal a.name b.name
  let hash s = Hashtbl.hash s.name
  let to_string s = s.name
end

module ConnectionEdge : Routing.EDGE with type t = connection = struct
  type t = connection
end

module FriendGraph = Routing.Graph.Make (StationNode) (ConnectionEdge)