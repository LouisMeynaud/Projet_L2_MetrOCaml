open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

(* ================================================== *)
(* 0. TYPES ET ÉTAT GLOBAL                            *)
(* ================================================== *)

(** Représente une plage horaire d'application d'une alerte, 
    définie par un décalage en secondes depuis minuit du jour ciblé. *)
type periode = { debut : int; fin : int }

(** Niveau de gravité d'une perturbation sur le réseau. 
    [BLOQUANTE] empêche totalement le passage par le tronçon.
    [PERTURBEE] ajoute une pénalité de temps dans le calcul de Dijkstra. *)
type severite = BLOQUANTE | PERTURBEE

(** Structure représentant une perturbation (alerte) brute en cours sur le réseau.
    Contient les informations textuelles pour l'affichage dans l'interface,
    ainsi que la liste des éléments physiques (lignes, stations, tronçons) touchés. *)
type alerte = {
  id : string;
  cause : string;
  niveau_severite : severite;
  titre : string;
  message : string;
  lignes_impactees : string list;
  sections_impactees : (string * string) list;
  stations_impactees : string list;
  temps : periode list;
}

(** Structure optimisée d'une alerte, spécifiquement taillée pour le moteur de routage Dijkstra.
    Utilise une table de hachage [stations_hash] pour une vérification d'impact en O(1). *)
type alerte_dijkstra= {
  severite : severite;
  stations_hash : (string, bool) Hashtbl.t; 
  periodes : periode list;
}

(** Référence globale stockant la liste des alertes actives. Agit comme la
    "Source de Vérité" dynamique en mémoire (RAM). Elle est lue par l'interface
    graphique et mise à jour en tâche de fond par le thread. *)
let liste_alertes : alerte list ref = ref []
let index_alertes_dijkstra : (string, alerte_dijkstra list) Hashtbl.t ref = ref (Hashtbl.create 0)
let mutex = Mutex.create ()

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
        Scanf.sscanf s "%4d%2d%2dT%2d%2d%2d" (fun y m d h min sec ->
            let tm =
              {
                Unix.tm_sec = sec;
                tm_min = min;
                tm_hour = h;
                tm_mday = d;
                tm_mon = m - 1;
                Unix.tm_year = y - 1900;
                tm_wday = 0;
                tm_yday = 0;
                tm_isdst = false;
              }
            in
            int_of_float (fst (Unix.mktime tm)))
      with _ -> 0

(** [string_to_severite str] convertit la chaîne renvoyée par l'API en type [severite]. *)
let string_to_severite = function
  | "BLOQUANTE" -> BLOQUANTE
  | _ -> PERTURBEE

(** [string_of_severite s] convertit le type [severite] en chaîne de caractères affichable. *)
let string_of_severite = function
  | BLOQUANTE -> "BLOQUANTE"
  | PERTURBEE -> "PERTURBEE"

(** [nettoyer_html s] purge une chaîne de caractères de ses balises HTML et décode 
    les entités HTML courantes (ex: &nbsp;, &amp;).
    Indispensable car l'API IDFM intègre parfois du formatage web dans ses messages. *)
let nettoyer_html s =
  let sans_balises = Str.global_replace (Str.regexp "<[^>]*>") "" s in
  sans_balises
  |> Str.global_replace (Str.regexp "&nbsp;") " " (*regexp car global_replace prend que des type regexp*)
  |> Str.global_replace (Str.regexp "&amp;") "&"
  |> Str.global_replace (Str.regexp "&#39;") "'"
  |> Str.global_replace (Str.regexp "&quot;") "\""
  |> Str.global_replace (Str.regexp "&lt;") "<"
  |> Str.global_replace (Str.regexp "&gt;") ">"

(** [format_alerte alerte] formate un objet [alerte] en une chaîne de caractères multilignes 
    et propre, prête à être injectée dans l'interface utilisateur ou le terminal. *)
let format_alerte alerte =
  let message = String.trim (nettoyer_html alerte.message) in
  let msg = if message = "" then "Message non renseigné" else message in
  let lignes = match alerte.lignes_impactees with
    | [] -> ["Inconnue"]
    | l -> l
  in
  let format_ligne ligne =
    Printf.sprintf "------\nLigne impactée: %s\nGravité: %s\nMessage: %s\n------"
      ligne (string_of_severite alerte.niveau_severite) msg
  in
  lignes
  |> List.map format_ligne
  |> String.concat "\n"
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
  let apiKey = match Sys.getenv_opt "IDFM_API_KEY" with
    | Some k -> k
    | None -> failwith "La clé API IDFM_API_KEY introuvable. Veuillez la définir dans le fichier .env"
  in
  let filename = "metrocaml/data/rt/alertes.json" in
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
  let filename = "metrocaml/data/rt/alertes.json" in
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
              let sections_troncons = ref [] in
              let sections =
                try disruption |> member "impactedSections" |> to_list
                with _ -> []
              in

              List.iter
                (fun section ->
                  try
                    let raw_line = section |> member "lineId" |> to_string in
                    lignes_touchees := nettoyer_id raw_line :: !lignes_touchees;
                    
                    (* MODIFICATION 1 : On extrait le 'from' et le 'to' pour remplir les tronçons *)
                    let f_id = nettoyer_id (section |> member "from" |> member "id" |> to_string) in
                    let t_id = nettoyer_id (section |> member "to" |> member "id" |> to_string) in
                    stations_touchees := f_id :: t_id :: !stations_touchees;
                    sections_troncons := (f_id, t_id) :: !sections_troncons;
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
                begin try
                  let obj_line = obj |> member "ptObject" |> member "line" |> member "id" |> to_string in
                  lignes_touchees := nettoyer_id obj_line :: !lignes_touchees
                with _ -> () end;
                begin try
                  let s_id = obj |> member "ptObject" |> member "stopPoint" |> member "id" |> to_string in
                  stations_touchees := nettoyer_id s_id :: !stations_touchees
                with _ -> () end
              ) impacted_objects;

              let lignes_uniques = List.sort_uniq String.compare !lignes_touchees in
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
                  sections_impactees = !sections_troncons; 
                  temps = periodes_actives;
                }
              in

              obj :: acc
            with _ -> acc)
          [] disruptions
      in
      Mutex.protect mutex (fun () -> liste_alertes := nouvelles_alertes)
    with _ ->
      Printf.printf "[Erreur] Impossible de parser le fichier %s.\n%!" filename
    end
  else
    Printf.printf "[Avertissement] Le fichier %s n'existe pas encore.\n%!"
      filename


      let trouver_gares_intermediaires graph line_id from_id to_id =
        let start_node_opt = ref None in
        List.iter (fun node -> if nettoyer_id node.Network.id = from_id then start_node_opt := Some node) (Network.TransportGraph.nodes graph);
      
        match !start_node_opt with
        | None -> []
        | Some start_node ->
            let queue = Queue.create () in Queue.push (start_node, [nettoyer_id start_node.Network.id]) queue;
            let visited = Hashtbl.create 16 in Hashtbl.add visited start_node.Network.id true;
            let result = ref [] in let found = ref false in
      
            while not (Queue.is_empty queue) && not !found do
              let (current_node, path) = Queue.pop queue in
              if nettoyer_id current_node.Network.id = to_id then begin result := path; found := true end 
              else begin
                let edges = Network.TransportGraph.neighbors graph current_node in
                List.iter (fun (neighbor, connection) ->
                  if nettoyer_id connection.Network.line.Network.id = nettoyer_id line_id then begin
                    if not (Hashtbl.mem visited neighbor.Network.id) then begin
                      Hashtbl.add visited neighbor.Network.id true;
                      Queue.push (neighbor, (nettoyer_id neighbor.Network.id) :: path) queue
                    end
                  end
                ) edges
              end
            done; !result


  (** [compiler_alertes_pour_dijkstra ()] crée un index (Table de hachage) où 
    la CLÉ est l'ID de la ligne, et la VALEUR est la liste de ses alertes actives. *)
    let compiler_alertes_pour_dijkstra graph =
      let index_lignes = Hashtbl.create 1000 in
      List.iter (fun alerte ->
        let gares_hash = Hashtbl.create 100 in
        List.iter (fun s -> Hashtbl.replace gares_hash s true) alerte.stations_impactees;
        
        List.iter (fun l_id ->
          List.iter (fun (f_id, t_id) ->
            let gares_cachees = trouver_gares_intermediaires graph l_id f_id t_id in
            List.iter (fun gc -> Hashtbl.replace gares_hash gc true) gares_cachees
          ) alerte.sections_impactees
        ) alerte.lignes_impactees;
    
        let alerte_opt = { severite = alerte.niveau_severite; stations_hash = gares_hash; periodes = alerte.temps; } in
        List.iter (fun l_id ->
          let alertes_existantes = try Hashtbl.find index_lignes l_id with Not_found -> [] in
          Hashtbl.replace index_lignes l_id (alerte_opt :: alertes_existantes)
        ) alerte.lignes_impactees
      ) !liste_alertes;
      index_lignes

(* ================================================== *)
(* 5. OUTIL DE DEBUGGAGE                              *)
(* ================================================== *)

(** Convertit le type personnalisé [severite] en texte *)
let severite_to_string = function
| BLOQUANTE -> "BLOQUANTE"
| PERTURBEE -> "PERTURBEE"

(** [exporter_index_debug ()] génère un fichier texte lisible par un humain 
  pour vérifier le contenu exact de la table de hachage de Dijkstra. *)
let exporter_index_debug () =
let filename = "data/rt/debug_dijkstra.txt" in
let oc = open_out filename in

Printf.fprintf oc "=== DUMP DE L'INDEX DIJKSTRA (TEMPS RÉEL) ===\n\n";

if Hashtbl.length !index_alertes_dijkstra = 0 then
  Printf.fprintf oc "L'index est totalement VIDE. (Aucune alerte active ou problème de compilation)\n"
else begin
  (* On parcourt toutes les lignes qui ont au moins un problème *)
  Hashtbl.iter (fun id_ligne liste_alertes ->
    Printf.fprintf oc "--------------------------------------------------\n";
    Printf.fprintf oc "LIGNE IMPACTÉE : %s\n" id_ligne;
    Printf.fprintf oc "--------------------------------------------------\n";
    
    List.iteri (fun i (alerte : alerte_dijkstra) ->
      Printf.fprintf oc "  [Alerte #%d] Sévérité : %s\n" (i + 1) (severite_to_string alerte.severite);
      (* On extrait les clés de la Hashtbl des stations pour les afficher *)
      let stations_list = Hashtbl.fold (fun id_station _ acc -> id_station :: acc) alerte.stations_hash [] in
      if stations_list = [] then
        Printf.fprintf oc "  -> Stations  : TOUTES (Ligne entière)\n"
      else
        Printf.fprintf oc "  -> Stations  : %s\n" (String.concat ", " stations_list);
      
      (* Affichage des périodes *)
      Printf.fprintf oc "  -> Périodes  : ";
      if alerte.periodes = [] then 
        Printf.fprintf oc "Immédiat / Sans limite de temps\n"
      else begin
        Printf.fprintf oc "\n";
        List.iter (fun p -> 
          Printf.fprintf oc "       De [T0 + %d sec] à [T0 + %d sec]\n" p.debut p.fin
        ) alerte.periodes
      end;
      Printf.fprintf oc "\n"
    ) liste_alertes;
    
  ) !index_alertes_dijkstra
end;

close_out oc;
Printf.printf "[Debug] Fichier %s généré avec succès ! Allez le lire pour vérifier vos données.\n%!" filename
      

(** [start_auto_refresh ()] initialise et lance le moteur Temps Réel en
    arrière-plan. Cette fonction crée un thread indépendant (fantôme) qui
    exécute en boucle : 1. Le téléchargement réseau (bloque uniquement ce
    thread, pas le jeu). 2. Le parsing du fichier vers la mémoire. 3. Une pause
    de 45 secondes. Doit être appelée une seule fois au démarrage de
    l'application. *)
let start_auto_refresh date graph =
  let rec loop () =
    actualiser_fichier_alertes ();
    charger_alertes_en_memoire date;
    let nouv_index = compiler_alertes_pour_dijkstra graph in
    Mutex.protect mutex (fun () -> index_alertes_dijkstra := nouv_index);
    (**DEBUG
    exporter_index_debug();**)
    Unix.sleep 60;
    loop ()
  in
  ignore (Thread.create loop ())

