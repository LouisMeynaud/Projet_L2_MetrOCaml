open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

(* ================================================== *)
(* 0. TYPES ET ÉTAT GLOBAL                            *)
(* ================================================== *)
type periode = { debut : int; fin : int }
type severite = BLOQUANTE | PERTURBEE

type alerte = {
  id : string;
  cause : string;
  niveau_severite : severite;
  titre : string;
  message : string;
  lignes_impactees : string list;
  stations_impactees : string list;
  temps : periode list;
}

type alerte_dijkstra= {
  severite : severite;
  stations_hash : (string, bool) Hashtbl.t; 
  periodes : periode list;
}

(** Structure représentant une perturbation (alerte) en cours sur le réseau.
    Contient les informations textuelles pour l'affichage dans l'interface,
    ainsi que la liste des lignes touchées. *)

(** Référence globale stockant la liste des alertes actives. Agit comme la
    "Source de Vérité" dynamique en mémoire (RAM). Elle est lue par l'interface
    graphique et mise à jour en tâche de fond par le thread. *)
let liste_alertes : alerte list ref = ref []
let index_alertes_dijkstra : (string, alerte_dijkstra list) Hashtbl.t ref = ref (Hashtbl.create 0)

let apiKey = "GBsIolemaspJPcsW7HbwQTp4RaiaBTQk"

(* ================================================== *)
(* 1. UTILITAIRES                                     *)
(* ================================================== *)

(** [nettoyer_id s] nettoie les identifiants bruts renvoyés par l'API IDFM.
    L'API préfixe souvent les IDs (ex: "line:IDFM:C01727"). Cette fonction isole
    et extrait uniquement la partie utile pour faire le lien avec le GTFS
    statique.

    @param s L'identifiant brut sous forme de chaîne de caractères.
    @return L'identifiant nettoyé (ex: "IDFM:C01727"). *)
let nettoyer_id s =
  let parts = String.split_on_char ':' s in
  match List.rev parts with
  | [] -> s
  | last_part :: _ -> last_part

(** [yyyymmdd_to_midnight s] convertit une date "YYYYMMDD" en timestamp Unix à
    00h00m00s *)
let yyyymmdd_to_midnight s =
  try
    Scanf.sscanf s "%4d%2d%2d" (fun y m d ->
        let tm =
          {
            Unix.tm_sec = 0;
            tm_min = 0;
            tm_hour = 0;
            tm_mday = d;
            tm_mon = m - 1;
            tm_year = y - 1900;
            tm_wday = 0;
            tm_yday = 0;
            tm_isdst = false;
          }
        in
        int_of_float (fst (Unix.mktime tm)))
  with _ -> 0

(** [date_to_int s] convertit une date au format ISO 8601 (ex:
    "2026-04-06T10:16:55") en timestamp Unix (nombre total de secondes écoulées
    depuis 1970).
    @param s La chaîne de caractères représentant la date.
    @return Le timestamp Unix (entier). Renvoie 0 en cas d'erreur de format. *)
let date_to_int s =
  try
    Scanf.sscanf s "%d-%d-%dT%d:%d:%d" (fun y m d h min sec ->
        let tm =
          {
            Unix.tm_sec = sec;
            tm_min = min;
            tm_hour = h;
            tm_mday = d;
            tm_mon = m - 1;
            tm_year = y - 1900;
            tm_wday = 0;
            tm_yday = 0;
            tm_isdst = false;
          }
        in
        int_of_float (fst (Unix.mktime tm)))
  with _ -> 0

let string_to_severite s =
  match s with
  | "PERTURBEE" -> PERTURBEE
  | "BLOQUANTE" -> BLOQUANTE
  | _ -> PERTURBEE

let string_of_severite = function
  | BLOQUANTE -> "BLOQUANTE"
  | PERTURBEE -> "PERTURBEE"

let string_of_severity = string_of_severite

let nettoyer_html s =
  let buffer = Buffer.create (String.length s) in
  let rec loop i in_tag =
    if i >= String.length s then ()
    else
      match s.[i] with
      | '<' -> loop (i + 1) true
      | '>' when in_tag -> loop (i + 1) false
      | _ when in_tag -> loop (i + 1) in_tag
      | '&' ->
          let rec find_semicolon j =
            if j >= String.length s then None
            else if s.[j] = ';' then Some j else find_semicolon (j + 1)
          in
          (match find_semicolon (i + 1) with
          | Some j ->
              let entity = String.sub s i (j - i + 1) in
              let decoded =
                match entity with
                | "&nbsp;" -> " "
                | "&amp;" -> "&"
                | "&lt;" -> "<"
                | "&gt;" -> ">"
                | "&quot;" -> "\""
                | "&#39;" -> "'"
                | _ -> entity
              in
              Buffer.add_string buffer decoded;
              loop (j + 1) in_tag
          | None ->
              Buffer.add_char buffer '&';
              loop (i + 1) in_tag)
      | c ->
          Buffer.add_char buffer c;
          loop (i + 1) in_tag
  in
  loop 0 false;
  Buffer.contents buffer

let format_alerte_pour_affichage (alerte : alerte) =
  let message =
    let message = String.trim (nettoyer_html alerte.message) in
    if message = "" then "Message non renseigne" else message
  in
  let lignes =
    match alerte.lignes_impactees with
    | [] -> ["Ligne inconnue"]
    | xs -> xs
  in
  String.concat "\n"
    (List.map
       (fun ligne ->
         [
           "------";
           Printf.sprintf "Ligne impactee: %s" ligne;
           Printf.sprintf "Gravite: %s" (string_of_severity alerte.niveau_severite);
           Printf.sprintf "Message: %s" message;
           "------";
         ]
         |> String.concat "\n")
       lignes)
(* ================================================== *)
(* 2. TÉLÉCHARGEMENT                                  *)
(* ================================================== *)

(** [fetch_and_save_disruptions ()] exécute une requête HTTP GET asynchrone vers
    l'API PRIM d'Île-de-France Mobilités pour récupérer les perturbations en
    cours. Le flux JSON reçu est écrit directement sur le disque (mode
    silencieux).
    @return
      Une promesse Lwt de type [unit] qui se résout une fois l'écriture
      terminée. *)
let recupere_alertes () =
  let filename = "data/rt/alertes.json" in
  let uri =
    Uri.of_string
      "https://prim.iledefrance-mobilites.fr/marketplace/disruptions_bulk/disruptions/v2"
  in
  let headers =
    Header.add_list (Header.init ())
      [ ("User-Agent", "insomnia/12.4.0"); ("apiKey", apiKey) ]
  in

  Client.call ~headers `GET uri >>= fun (res, body_stream) ->
  let code = res |> Response.status |> Code.code_of_status in

  if code = 200 then (
    Cohttp_lwt.Body.to_string body_stream >>= fun body_str ->
    (* --- ÉCRITURE DANS LE FICHIER --- *)
    let oc = open_out filename in
    output_string oc body_str;
    close_out oc;
    Lwt.return_unit)
  else
    Cohttp_lwt.Body.drain_body body_stream >>= fun () ->
    Printf.printf "Erreur lors du téléchargement de l'API (Code HTTP %d)\n%!"
      code;
    Lwt.return_unit

(** [actualiser_fichier_alertes ()]Elle exécute la promesse Lwt de
    téléchargement et force le thread courant à attendre que le fichier JSON
    soit complètement mis à jour sur le disque. *)
let actualiser_fichier_alertes () = Lwt_main.run (recupere_alertes ())

(* ================================================== *)
(* 3. LECTURE ET PARSING DU JSON                      *)
(* ================================================== *)

(** [charger_alertes_en_memoire ()] lit le fichier JSON local, parse
    l'arborescence des perturbations, et remplace le contenu de [liste_alertes]
    par les nouvelles données.

    L'algorithme extrait les informations utiles (titre, message, cause) et
    descend dans les "impactedSections" pour recenser les identifiants uniques
    des lignes touchées. L'utilisation de blocs try/with garantit que le
    programme ne plantera pas si une alerte spécifique est mal formattée par le
    serveur IDFM. *)
let charger_alertes_en_memoire date_cible =
  let filename = "data/rt/alertes.json" in
  let date_midnight = yyyymmdd_to_midnight date_cible in
  if Sys.file_exists filename then
    begin try
      let json = Yojson.Basic.from_file filename in

      (* On rentre dans le tableau "disruptions" *)
      let disruptions = json |> member "disruptions" |> to_list in

      let nouvelles_alertes =
        List.fold_left
          (fun acc disruption ->
            try
              (* --- 1. Extraction des infos de base --- *)
              let d_id = disruption |> member "id" |> to_string in
              let d_cause =
                try disruption |> member "cause" |> to_string
                with _ -> "Inconnue"
              in
              let d_severity =
                try disruption |> member "severity" |> to_string
                with _ -> "PERTURBATION"
              in
              let d_title =
                try disruption |> member "title" |> to_string
                with _ -> "Information Trafic"
              in
              let d_message =
                try disruption |> member "message" |> to_string with _ -> ""
              in

              (* --- 2. Extraction des lignes impactées --- *)
              let lignes_touchees = ref [] in
              let stations_touchees = ref [] in

              let sections =
                try disruption |> member "impactedSections" |> to_list
                with _ -> []
              in

              List.iter
                (fun section ->
                  try
                    let raw_line = section |> member "lineId" |> to_string in
                    lignes_touchees := nettoyer_id raw_line :: !lignes_touchees;
                    let parse_stop node_name =
                      try
                        let s_id =
                          section |> member node_name |> member "id"
                          |> to_string
                        in
                        stations_touchees :=
                          nettoyer_id s_id :: !stations_touchees
                      with _ -> ()
                    in
                    parse_stop "from";
                    parse_stop "to"
                  with _ -> ())
                sections;

              let periodes_actives =
                try
                  let apps =
                    disruption |> member "applicationPeriods" |> to_list
                  in
                  List.fold_left
                    (fun acc_p p ->
                      try
                        let p_debut =
                          p |> member "begin" |> to_string |> date_to_int
                        in
                        let p_fin =
                          p |> member "end" |> to_string |> date_to_int
                        in
                        {
                          debut = p_debut - date_midnight;
                          fin = p_fin - date_midnight;
                        }
                        :: acc_p
                      with _ -> acc_p)
                    [] apps
                with _ -> []
              in

              let stops_directs = 
                try disruption |> member "impactedStops" |> to_list 
                with _ -> [] 
              in
              List.iter (fun stop ->
                try
                  let s_id = stop |> member "stopPoint" |> member "id" |> to_string in
                  stations_touchees := nettoyer_id s_id :: !stations_touchees
                with _ -> ()
              ) stops_directs;

              (* 2. On cherche dans impactedObjects (Le format standard IDFM) *)
              let impacted_objects = 
                try disruption |> member "impactedObjects" |> to_list 
                with _ -> [] 
              in
              List.iter (fun obj ->
                (* On récupère la ligne si elle est là *)
                begin try
                  let obj_line = obj |> member "ptObject" |> member "line" |> member "id" |> to_string in
                  lignes_touchees := nettoyer_id obj_line :: !lignes_touchees
                with _ -> () end;

                (* On récupère TOUTES les gares du tronçon ! *)
                let obj_stops = try obj |> member "impactedStops" |> to_list with _ -> [] in
                List.iter (fun stop ->
                  try
                    let s_id = stop |> member "stopPoint" |> member "id" |> to_string in
                    stations_touchees := nettoyer_id s_id :: !stations_touchees
                  with _ -> ()
                ) obj_stops
              ) impacted_objects;

              let lignes_uniques = List.sort_uniq String.compare !lignes_touchees in
              (* On enlève aussi les doublons dans les gares *)
              let stations_uniques = List.sort_uniq String.compare !stations_touchees in
              (* --- 3. Création de l'objet --- *)
              let obj =
                {
                  id = d_id;
                  cause = d_cause;
                  niveau_severite = string_to_severite d_severity;
                  titre = d_title;
                  message = d_message;
                  lignes_impactees = lignes_uniques;
                  stations_impactees = stations_uniques;
                  temps = periodes_actives;
                }
              in

              obj :: acc
            with _ -> acc)
          [] disruptions
      in
      liste_alertes := nouvelles_alertes
    with _ ->
      Printf.printf "[Erreur] Impossible de parser le fichier %s.\n%!" filename
    end
  else
    Printf.printf "[Avertissement] Le fichier %s n'existe pas encore.\n%!"
      filename



  (** [compiler_alertes_pour_dijkstra ()] crée un index (Table de hachage) où 
    la CLÉ est l'ID de la ligne, et la VALEUR est la liste de ses alertes actives. *)
    let compiler_alertes_pour_dijkstra () =
      let index_lignes = Hashtbl.create 1000 in
      
      List.iter (fun alerte ->
        let gares_hash = Hashtbl.create 100 in
        List.iter (fun s -> Hashtbl.replace gares_hash s true) alerte.stations_impactees;
        let alerte_opt = {
          severite = alerte.niveau_severite;
          stations_hash = gares_hash;
          periodes = alerte.temps;
        } in
        List.iter (fun l_id ->
          let alertes_existantes = try Hashtbl.find index_lignes l_id with Not_found -> [] in
          Hashtbl.replace index_lignes l_id (alerte_opt :: alertes_existantes)
        ) alerte.lignes_impactees
      ) !liste_alertes;
      
      index_lignes

      
(** [start_auto_refresh ()] initialise et lance le moteur Temps Réel en
    arrière-plan. Cette fonction crée un thread indépendant (fantôme) qui
    exécute en boucle : 1. Le téléchargement réseau (bloque uniquement ce
    thread, pas le jeu). 2. Le parsing du fichier vers la mémoire. 3. Une pause
    de 45 secondes. Doit être appelée une seule fois au démarrage de
    l'application. *)
let start_auto_refresh date =
  let rec loop () =
    actualiser_fichier_alertes ();
    charger_alertes_en_memoire date;
    index_alertes_dijkstra := compiler_alertes_pour_dijkstra ();
    Unix.sleep 60;
    loop ()
  in
  ignore (Thread.create loop ())

