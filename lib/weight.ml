open Network

(** Constante de pénalité de transfert globale (en secondes) *)
let transfer_penalty = 180

(** Helper magique pour formater le temps en chaîne lisible *)
let time_to_string time =
  if time = max_int then "∞"
  else
    let days = time / 86400 in
    let hours = time mod 86400 / 3600 in
    let minutes = time mod 3600 / 60 in
    let seconds = time mod 60 in
    match days with
    | 0 -> Printf.sprintf "Aujourd'hui à %02dh%02dm%02ds" hours minutes seconds
    | 1 -> Printf.sprintf "Demain à %02dh%02dm%02ds" hours minutes seconds
    | 2 -> Printf.sprintf "Après-demain à %02dh%02dm%02ds" hours minutes seconds
    | _ -> Printf.sprintf "J+%d à %02dh%02dm%02ds" days hours minutes seconds

(** Helper global pour calculer la prochaine heure de départ pour n'importe quel
    poids *)
let calc_next_time current_time current_line_name (edge : connection) =
  if current_time = max_int then (max_int, None)
  else if edge.mode = Walk || edge.mode = Transfert then (current_time + edge.duration, current_line_name)
  else
    let is_transfer =
      match current_line_name with
      | Some l when l <> edge.line.name -> true
      | _ -> false
    in
    let ready_time =
      if is_transfer then current_time + transfer_penalty else current_time
    in
    let rec find_next_departure = function
      | [] -> max_int
      | t :: ts -> if t >= ready_time then t else find_next_departure ts
    in
    let next_dep = find_next_departure edge.schedules in
    if next_dep = max_int then (max_int, None)
    else (next_dep + edge.duration, Some edge.line.name)

(* ========================================================== *)
(* LES MODULES DE POIDS *)
(* ========================================================== *)

module TimeWeight : Routing.WEIGHT with type edge = connection = struct
  type t = { time : int; line_name : string option }
  type edge = connection

  let start_at time = { time; line_name = None }
  let infinity = { time = max_int; line_name = None }
  let is_inf w = w.time = max_int

  let add w e =
    let next_time, next_line = calc_next_time w.time w.line_name e in
    { time = next_time; line_name = next_line }

  let compare w1 w2 =
    if is_inf w1 && is_inf w2 then 0.0
    else if is_inf w1 then 1.0
    else if is_inf w2 then -1.0
    else
      let diff = w1.time - w2.time in
      float_of_int diff /. float_of_int (Int.max 1 (Int.max w1.time w2.time))

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0
  let get_time w = w.time
  let to_string w = time_to_string w.time
end

module DistanceWeight : Routing.WEIGHT with type edge = connection = struct
  type t = { time : int; distance : float; line_name : string option }
  type edge = connection

  let start_at time = { time; distance = 0.0; line_name = None }
  let infinity = { time = max_int; distance = Float.infinity; line_name = None }
  let is_inf w = w.time = max_int

  let add w e =
    let next_time, next_line = calc_next_time w.time w.line_name e in
    if next_time = max_int then infinity
    else
      {
        time = next_time;
        distance = w.distance +. e.distance;
        line_name = next_line;
      }

  let compare w1 w2 =
    if is_inf w1 && is_inf w2 then 0.0
    else if is_inf w1 then 1.0
    else if is_inf w2 then -1.0
    else
      let diff = w1.distance -. w2.distance in
      if Float.abs diff < 0.1 then (* Tie-breaker sur le temps *)
        float_of_int (w1.time - w2.time)
        /. float_of_int (Int.max 1 (Int.max w1.time w2.time))
      else diff /. Float.max 1.0 (Float.max w1.distance w2.distance)

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0
  let get_time w = w.time

  let to_string w =
    if is_inf w then "∞"
    else if w.distance >= 1000.0 then
      Printf.sprintf "%.1f km (%s)" (w.distance /. 1000.0)
        (time_to_string w.time)
    else Printf.sprintf "%.0f m (%s)" w.distance (time_to_string w.time)
end

module HopWeight : Routing.WEIGHT with type edge = connection = struct
  type t = { time : int; hops : int; line_name : string option }
  type edge = connection

  let start_at time = { time; hops = 0; line_name = None }
  let infinity = { time = max_int; hops = max_int; line_name = None }
  let is_inf w = w.time = max_int

  let add w e =
    let next_time, next_line = calc_next_time w.time w.line_name e in
    if next_time = max_int then infinity
    else { time = next_time; hops = w.hops + 1; line_name = next_line }

  let compare w1 w2 =
    if is_inf w1 && is_inf w2 then 0.0
    else if is_inf w1 then 1.0
    else if is_inf w2 then -1.0
    else
      let diff = w1.hops - w2.hops in
      if diff = 0 then
        float_of_int (w1.time - w2.time)
        /. float_of_int (Int.max 1 (Int.max w1.time w2.time))
      else
        float_of_int diff /. float_of_int (Int.max 1 (Int.max w1.hops w2.hops))

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0
  let get_time w = w.time

  let to_string w =
    if is_inf w then "∞"
    else Printf.sprintf "%d arrêts (%s)" w.hops (time_to_string w.time)
end

module CorrespondenceWeight : Routing.WEIGHT with type edge = connection =
struct
  type t = { time : int; transfers : int; line_name : string option }
  type edge = connection

  let start_at time = { time; transfers = 0; line_name = None }
  let infinity = { time = max_int; transfers = max_int; line_name = None }
  let is_inf w = w.time = max_int

  let add w e =
    let next_time, next_line = calc_next_time w.time w.line_name e in
    if next_time = max_int then infinity
    else
      let is_transfer =
        match w.line_name with Some l when l <> e.line.name -> 1 | _ -> 0
      in
      {
        time = next_time;
        transfers = w.transfers + is_transfer;
        line_name = next_line;
      }

  let compare w1 w2 =
    if is_inf w1 && is_inf w2 then 0.0
    else if is_inf w1 then 1.0
    else if is_inf w2 then -1.0
    else
      let diff = w1.transfers - w2.transfers in
      if diff = 0 then
        float_of_int (w1.time - w2.time)
        /. float_of_int (Int.max 1 (Int.max w1.time w2.time))
      else
        float_of_int diff
        /. float_of_int (Int.max 1 (Int.max w1.transfers w2.transfers))

  let ( < ) a b = compare a b < 0.0
  let ( > ) a b = compare a b > 0.0
  let ( = ) a b = compare a b = 0.0
  let get_time w = w.time

  let to_string w =
    if is_inf w then "∞"
    else
      Printf.sprintf "%d correspondances (%s)" w.transfers
        (time_to_string w.time)
end
