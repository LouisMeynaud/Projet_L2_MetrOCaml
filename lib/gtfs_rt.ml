open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

(* ================================================== *)
(* 0. TYPES ET ÉTAT GLOBAL                            *)
(* ================================================== *)
type alerte = {
  id : string;
  cause : string;
  severite : string;
  titre : string;
  message : string;
  lignes_impactees : string list;
}

let liste_alertes : alerte list ref = ref []

(* ================================================== *)
(* 1. UTILITAIRES                                     *)
(* ================================================== *)
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
        int_of_float (fst (Unix.mktime tm))
      )
    with _ -> 0

(* ================================================== *)
(* 2. TÉLÉCHARGEMENT                                  *)
(* ================================================== *)
let fetch_and_save_disruptions =
  let filename = "data/rt/alertes.json" in
  let uri = Uri.of_string "https://prim.iledefrance-mobilites.fr/marketplace/disruptions_bulk/disruptions/v2" in
  let headers = Header.add_list (Header.init ()) [
    ("User-Agent", "insomnia/12.4.0");
    ("apiKey", "PUT UR API KEY HERE");
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
      Lwt.return_unit
      
    else
      Cohttp_lwt.Body.drain_body body_stream
      >>= fun () ->
      Printf.printf "Erreur lors du téléchargement de l'API (Code HTTP %d)\n%!" code;
      Lwt.return_unit

(* 2. La fonction  à appeler dans ton code principal *)
let actualiser_fichier_alertes () =
  Lwt_main.run (fetch_and_save_disruptions)

(* ================================================== *)
(* 3. LECTURE ET PARSING DU JSON                      *)
(* ================================================== *)

let charger_alertes_en_memoire () =
  let filename = "data/rt/alertes.json" in
  if Sys.file_exists filename then begin
    try
      let json = Yojson.Basic.from_file filename in
      
      (* On rentre dans le tableau "disruptions" *)
      let disruptions = json |> member "disruptions" |> to_list in
      
      let nouvelles_alertes = List.fold_left (fun acc disruption ->
        try
          (* --- 1. Extraction des infos de base --- *)
          let d_id = disruption |> member "id" |> to_string in
          let d_cause = try disruption |> member "cause" |> to_string with _ -> "Inconnue" in
          let d_severity = try disruption |> member "severity" |> to_string with _ -> "PERTURBATION" in
          let d_title = try disruption |> member "title" |> to_string with _ -> "Information Trafic" in
          let d_message = try disruption |> member "message" |> to_string with _ -> "" in
          
          (* --- 2. Extraction des lignes impactées --- *)
          let lignes_touchees = ref [] in
          
          let sections = 
            try disruption |> member "impactedSections" |> to_list 
            with _ -> [] 
          in
          
          List.iter (fun section ->
            try
              let raw_line = section |> member "lineId" |> to_string in
              lignes_touchees := (nettoyer_id raw_line) :: !lignes_touchees
            with _ -> ()
          ) sections;
          
          (* On enlève les doublons si l'API liste l'aller et le retour *)
          let lignes_uniques = List.sort_uniq String.compare !lignes_touchees in
          
          (* --- 3. Création de l'objet --- *)
          let obj = {
            id = d_id;
            cause = d_cause;
            severite = d_severity;
            titre = d_title;
            message = d_message;
            lignes_impactees = lignes_uniques;
          } in
          
          obj :: acc
          
        with _ -> acc 
      ) [] disruptions in
      liste_alertes := nouvelles_alertes;
    with _ -> Printf.printf "[Erreur] Impossible de parser le fichier %s.\n%!" filename
  end else
    Printf.printf "[Avertissement] Le fichier %s n'existe pas encore.\n%!" filename


    let start_auto_refresh () =
      let rec loop () =
        actualiser_fichier_alertes ();
        charger_alertes_en_memoire ();
        Unix.sleep 45;
        loop ()
      in
      ignore (Thread.create loop ())