open Routing
open Network

let hex_to_rgb hex =
  let hex =
    if String.length hex > 0 && hex.[0] = '#' then
      String.sub hex 1 (String.length hex - 1)
    else if hex = "" then "000000"
    else hex
  in
  let r = int_of_string ("0x" ^ String.sub hex 0 2) in
  let g = int_of_string ("0x" ^ String.sub hex 2 2) in
  let b = int_of_string ("0x" ^ String.sub hex 4 2) in
  (r, g, b)

let colorize_rgb use_color (r, g, b) text =
  if use_color then Printf.sprintf "\x1b[38;2;%d;%d;%dm%s\x1b[0m" r g b text
  else text

let colorize_hex use_color hex text =
  let rgb = hex_to_rgb hex in
  colorize_rgb use_color rgb text

let bold use_color text =
  if use_color then Printf.sprintf "\x1b[1m%s\x1b[0m" text else text

let italic use_color text =
  if use_color then Printf.sprintf "\x1b[3;90m%s\x1b[0m" text else text

let format_hhmm t =
  Printf.sprintf "%02d:%02d" (t / 3600 mod 24) (t / 60 mod 60)

let format_hms t =
  Printf.sprintf "%dh %dmin %ds" (t / 3600) (t / 60 mod 60) (t mod 60)

let format_min t =
  Printf.sprintf "~ %d min" (max 1 (t / 60))


module Make (P : Routing.PATH with type Node.t = station and type Edge.t = connection) = struct
  open P

  (* Liaison à pied *)
  let is_walklike (e : connection) = e.mode = Walk || e.mode = Transfert

  (* Un segment est une suite d'arêtes de même type et ligne.
     Par exemple, si on prend le RER A de Nation à Châtelet,
     on fusionnera les arêtes en un seul segment RER A : Nation -> Châtelet. 
  *)
  type segment_kind =
    | WalkSeg
    | TransitSeg of { line : line; mode : transport_mode }

  type segment = {
    kind          : segment_kind;
    start_station : station;
    end_station   : station;
    depart_time   : int;
    arrival_time  : int;
    hops          : int;
    distance      : float;
  }

  (* Si deux arêtes appartiennent au même segment *)
  let same_segment (e1 : connection) (e2 : connection) =
    if is_walklike e1 && is_walklike e2 then true
    else if (not (is_walklike e1)) && (not (is_walklike e2)) then e1.line.name = e2.line.name
    else false
  
  let build_segment (steps : step list): segment =
    let first = List.hd steps in
    let kind  = if is_walklike first.edge then WalkSeg
                else TransitSeg { line = first.edge.line; mode = first.edge.mode }
    in
    let distance, hops, last =
      List.fold_left (fun (d, n, _) s -> (d +. s.edge.distance, n + 1, s)) (0.0, 0, first) steps
    in
    {
      kind;
      start_station = first.from;
      end_station   = last.to_node;
      depart_time   = Weight.get_weight first.weight - first.edge.duration;
      arrival_time  = Weight.get_weight last.weight;
      hops;
      distance;
    }

  (* Regroupement les étapes consécutives *)
  let group_into_segments (steps : step list): segment list =
    let rec aux acc_rev = function
      | [] ->
          if acc_rev = [] then []
          else [ build_segment (List.rev acc_rev) ]
      | step :: rest ->
          match acc_rev with
          | [] -> aux [ step ] rest
          | hd :: _ ->
              (* si l'arête courante appartient au même segment que les arêtes accumulées, on l'empile *)
              if same_segment hd.edge step.edge then
                aux (step :: acc_rev) rest
              (* sinon on construit le segment et en commence un nouveau *)
              else
                let seg = build_segment (List.rev acc_rev) in
                seg :: aux [ step ] rest
    in
    aux [] steps

  
  (* Construction de la description d'un segment *)
  let render_segment_desc ~use_color (seg : segment) =
    let duration = format_min (seg.arrival_time - seg.depart_time) in
    match seg.kind with
    | WalkSeg ->
        italic use_color (Printf.sprintf "Marche %s" duration)
    | TransitSeg { line; mode } ->
        let label = Printf.sprintf "%s %s" (string_of_mode mode) line.name in
        let colored = colorize_hex use_color line.color label in
        let stop = if seg.hops <= 1 then "arrêt" else "arrêts" in
        Printf.sprintf "%s | %d %s %s" colored seg.hops stop duration

  (* Construction de la description du chemin *)
  let render_path ~start_time ~use_color (path : t) =
    match group_into_segments path.steps with
    | [] -> ""
    | segs ->
      let lines = ref [] in
      let add s = lines := s :: !lines in
      let add_station t name =
        add (Printf.sprintf "%s %s" (format_hhmm t) (bold use_color name))
      in
      let prev_arrival = ref start_time in
      List.iteri (fun i seg ->
        let wait = seg.depart_time - !prev_arrival in
        if i = 0 then add_station start_time seg.start_station.name;
        if wait > 0 then begin
          add (Printf.sprintf "     %s" (italic use_color (Printf.sprintf "Attente %s" (format_min wait))));
          add_station seg.depart_time seg.start_station.name
        end;
        add (Printf.sprintf "     %s" (render_segment_desc ~use_color seg));
        add_station seg.arrival_time seg.end_station.name;
        prev_arrival := seg.arrival_time
      ) segs;
      String.concat "\n" (List.rev !lines)


  type summary = {
    arrival_time : int;
    total_time : int;
    onboard_time : int;
    walking_time : int;
    waiting_time : int;
    total_distance : float;
    transfers : int;
  }

  (* Crée une synthèse du chemin avec les stats *)
  let compute_summary ~start_time (path : t) =
    let segs = group_into_segments path.steps in
    let arrival_time =
      List.fold_left (fun _ (seg : segment) -> seg.arrival_time) start_time segs
    in
    let total_time = arrival_time - start_time in
    let (onboard_time, walking_time, transit_count, total_distance) =
      List.fold_left (fun (ot, wt, tc, d) (seg:segment) ->
        let dur = seg.arrival_time - seg.depart_time in
        match seg.kind with
        | WalkSeg      -> (ot, wt + dur, tc, d)
        | TransitSeg _ -> (ot + dur, wt, tc + 1, d +. seg.distance)
      ) (0, 0, 0, 0.0) segs
    in
    {
      arrival_time;
      total_time;
      onboard_time;
      walking_time;
      waiting_time = max 0 (total_time - onboard_time - walking_time);
      total_distance;
      transfers = max 0 (transit_count - 1);
    }

  (* Renvoie la description du chemin fourni sous forme de string *)
  let string_of_path ~start_time ~use_color (path : t) =
    let route   = render_path ~start_time ~use_color path in
    let summary = compute_summary ~start_time path in
    match route with
    | "" -> "Chemin vide"
    | _  ->
        String.concat "\n"
          [
            route;
            "";
            Printf.sprintf "Arrivée : %s"         (format_hhmm summary.arrival_time);
            Printf.sprintf "Temps total : %s"     (format_hms  summary.total_time);
            Printf.sprintf "Temps à bord : %s"    (format_hms  summary.onboard_time);
            Printf.sprintf "Temps de marche : %s" (format_hms  summary.walking_time);
            Printf.sprintf "Temps d'attente : %s" (format_hms  summary.waiting_time);
            Printf.sprintf "Distance totale : %.2f km" (summary.total_distance /. 1000.);
            Printf.sprintf "Correspondances : %d" summary.transfers;
          ] ^ "\n\n"
end
