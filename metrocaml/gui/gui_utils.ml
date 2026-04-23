(* gui_utils.ml *)
open Bogue

let normalize s = String.lowercase_ascii (String.trim s) (* Supprime les espaces et met en minuscules pour une comparaison insensible à la casse *)


let contains_ci text pattern =
  try
    ignore (Str.search_forward (Str.regexp_string (normalize pattern)) (normalize text) 0); (* crash si pas trouvé *)
    true
  with Not_found -> false (* Si la recherche échoue, on rattrape l'erreur et retourne false *)

let now_seconds_of_day () =
  let tm = Unix.localtime (Unix.time ()) in
  (tm.Unix.tm_hour * 3600) + (tm.Unix.tm_min * 60) + tm.Unix.tm_sec

let format_seconds secs =
  let day = secs / 86_400 in
  let rem = secs mod 86_400 in
  let h = rem / 3600 in
  let m = (rem mod 3600) / 60 in
  if day <= 0 then Printf.sprintf "%02d:%02d" h m
  else Printf.sprintf "%02d:%02d (+%dj)" h m day