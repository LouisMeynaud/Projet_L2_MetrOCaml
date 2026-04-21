open Tiktocaml

module DijkstraForce = Routing.Dijkstra.Make (Network.FriendGraph) (Weight.Forceheight)
module Bfs = Routing.Bfs.Make (Network.FriendGraph) (Weight.Forceheight)

let find_person name =
  let name_ = String.lowercase_ascii (String.trim name) in
  List.find_opt (fun (p : Network.person) -> String.lowercase_ascii p.name = name_) Graphe.people

let () =
  let graph = Graphe.create_test_graph () in

  Printf.printf "Algo (dijkstra/bfs) : \n";
  let algo = String.lowercase_ascii (String.trim (read_line ())) in

  Printf.printf "Nom de depart : \n";
  let src = find_person (read_line ()) in

  Printf.printf "nom d'arrive : \n";
  let dst = find_person (read_line ()) in

  match src, dst with
  | None, _ | _, None -> 
      Printf.printf " nom inconnu\n"
      
  | Some source, Some destination ->
      if algo = "bfs" then begin
        match Bfs.find_path ~start_time:0 ~graph ~source ~destination with
        | None -> Printf.printf "Aucun chemin trouvé.\n"
        | Some p ->
          List.iter (fun (s : Bfs.P.step) -> Printf.printf "%s -> %s\n" s.from.name s.to_node.name) p.steps;
            Printf.printf "poids total: %s\n" (Bfs.P.Weight.to_string p.total_weight)
      end 
      
      else begin
        match DijkstraForce.find_path ~start_time:0 ~graph ~source ~destination ~transfer_allowed:(fun _ -> true) with
        | None -> Printf.printf "Aucun chemin trouve.\n"
        | Some p ->
            List.iter (fun (s : DijkstraForce.P.step) -> Printf.printf "%s -> %s\n" s.from.name s.to_node.name) p.steps;
            Printf.printf "poids total: %s\n" (DijkstraForce.P.Weight.to_string p.total_weight)
      end