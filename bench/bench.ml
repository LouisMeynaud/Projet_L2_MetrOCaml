open Metrocaml

let separator () =
  print_string (String.make 70 '-');
  print_newline ()

let format_duration secs =
  let ms = secs *. 1000.0 in
  Printf.sprintf "%.3f ms" ms


let run graphe source_query dest_query start_time
    (routeurs : (string * (Network.TransportGraph.t -> Network.station -> Network.station -> int -> string * (unit -> unit))) list)
    raw_select =
  Printf.printf "\nBenchmark de %s à %s \n" source_query dest_query;
  match Endpoint.build_routing_context graphe source_query dest_query with
  | None ->
    Logger.bold_red "Stations introuvable, benchmark annulé \n"
  | Some (graph_for_search, source, destination) ->
    Printf.printf "\n%-30s  %-12s  %-15s %-20s\n%!" "Algorithme/Preset" "Temps" "Resultat" "Temps du Chemin";
    separator ();

    let afficheurs =
      List.filter_map (fun (nom, search_fn) ->
        let t0 = Unix.gettimeofday () in
        let (result_str, print_detail) = search_fn graph_for_search source destination start_time in
        let t1 = Unix.gettimeofday () in
        Printf.printf "%-30s  %-12s  %s\n%!" nom (format_duration (t1 -. t0)) result_str;
        match result_str with
        | "Aucun chemin" -> None
        | _ -> Some (nom, print_detail)
      ) routeurs
    in
    separator ();

    if afficheurs <> [] then begin
      let choix = raw_select "\nAfficher les details des chemins ?" [ "Oui"; "Non" ] in
      if choix = "Oui" then begin
        Printf.printf "\n== Details des chemins : ==============\n%!";
        List.iter (fun (nom, print_detail) ->
          Printf.printf "-------> %s\n%!" nom;
          print_detail ();
          separator ()
        ) afficheurs;
      end
    end