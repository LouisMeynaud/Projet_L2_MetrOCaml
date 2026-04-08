open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix

(* 0. Utiliraires *)
let nettoyer_id s =
  let parts = String.split_on_char ':' s in
  let rec find_num = function
    | [] -> s 
    | h :: t ->
        if String.length h > 0 && h.[0] >= '0' && h.[0] <= '9' then h
        else find_num t
  in
  find_num parts

  let date_to_int s =
    try
      Scanf.sscanf s "%d-%d-%dT%d:%d:%d" (fun y m d h min sec ->
        let tm = { 
          Unix.tm_sec=sec; tm_min=min; tm_hour=h; tm_mday=d; 
          tm_mon=m-1; tm_year=y-1900; tm_wday=0; tm_yday=0; tm_isdst=false 
        } in
        fst (Unix.mktime tm)
      )
    with _ -> 0.0

(* 1. La fonction qui télécharge et écrit dans le fichier *)
let fetch_and_save_disruptions filename =
  let uri = Uri.of_string "https://prim.iledefrance-mobilites.fr/marketplace/disruptions_bulk/disruptions/v2" in
  let headers = Header.add_list (Header.init ()) [
    ("User-Agent", "insomnia/12.4.0");
    ("apiKey", "xt1T5slHhtOGGTo61qd7jWT0kOTwRQKv");
  ] in

  Client.call ~headers `GET uri
  >>= fun (res, body_stream) ->
    let code = res |> Response.status |> Code.code_of_status in
    
    if code = 200 then
      Cohttp_lwt.Body.to_string body_stream
      >>= fun body_str ->
      
      (* --- ÉCRITURE DANS LE FICHIER --- *)
      let oc = open_out filename in
      output_string oc body_str;
      close_out oc;
      
      Printf.printf "Succès : %s généré avec succès (%d octets) !\n%!" filename (String.length body_str);
      Lwt.return_unit
      
    else
      Cohttp_lwt.Body.drain_body body_stream
      >>= fun () ->
      Printf.printf "Erreur lors du téléchargement de l'API (Code HTTP %d)\n%!" code;
      Lwt.return_unit

(* 2. La fonction  à appeler dans ton code principal *)
let actualiser_fichier_alertes () =
  Lwt_main.run (fetch_and_save_disruptions "../data/rt/alertes.json")