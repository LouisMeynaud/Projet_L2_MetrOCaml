open Network


let lorenzo = { name = "Lorenzo"; age = 19 }
let leo = { name = "Leo"; age = 20 }
let matthys = { name = "Matthys"; age = 19 }
let louis = { name = "louis"; age = 19 }
let baptiste = { name = "Baptiste"; age = 19 }
let thomas = { name = "Thomas"; age = 20 }

let people = [ lorenzo; leo; matthys; louis; baptiste; thomas ]

let connect g a b force =
  FriendGraph.add_edge g a b { force }

let create_test_graph () =
  let g = FriendGraph.create () in
  List.iter (FriendGraph.add_node g) people;
  connect g lorenzo leo 1.0;
  connect g lorenzo matthys 0.8;
  connect g leo louis 0.7;
  connect g matthys baptiste 0.9;
  connect g louis thomas 0.6;
  connect g baptiste thomas 0.95;
  connect g leo baptiste 0.5;

  g
