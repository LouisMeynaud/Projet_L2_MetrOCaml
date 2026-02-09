(* gtfs_adapter.ml *)

open Network


let time_to_seconds time_str =
  try
    match String.split_on_char ':' (String.trim time_str) with
    | [h; m; s] -> (int_of_string h * 3600) + (int_of_string m * 60) + int_of_string s
    | _ -> 0
  with _ -> 0


let duration_between arrival_str departure_str =
  let diff = time_to_seconds arrival_str - time_to_seconds departure_str in
  if diff < 0 then 0 else diff
