open Metrocaml
open Weight
open Routing

type preset = {
  name : string;
  description : string;
  criteria : (Network.connection, Network.station) Combinator.criterion list;
}

(* Preset 1 : Rapide 
  Priorise le temps de trajet *)
let preset_rapide =
  {
    name = "Rapide";
    description = "Privilégie le temps de trajet.";
    criteria =
      [
        Combinator.criterion
          (module TimeWeight)
          ~coefficient:0.75 ~label:"Temps";
        Combinator.criterion
          (module CorrespondenceWeight)
          ~coefficient:0.25 ~label:"Correspondances";
        Combinator.criterion
          (module HopWeight)
          ~coefficient:0.00 ~label:"Arrêts";
        Combinator.criterion
          (module DistanceWeight)
          ~coefficient:0.00 ~label:"Distance";
      ];
  }

(* Preset 2 : Confort 
    Priorise les trajets avec peu de correspondances *)
let preset_confort =
  {
    name = "Confort";
    description = "Minimise les changements de ligne et le nombre d'arrêts.";
    criteria =
      [
        Combinator.criterion
          (module CorrespondenceWeight)
          ~coefficient:0.50 ~label:"Correspondances";
        Combinator.criterion
          (module HopWeight)
          ~coefficient:0.00 ~label:"Arrêts";
        Combinator.criterion
          (module TimeWeight)
          ~coefficient:0.50 ~label:"Temps";
        Combinator.criterion
          (module DistanceWeight)
          ~coefficient:0.00 ~label:"Distance";
      ];
  }

(* Preset 3 : Court chemin 
    Priorise la distance géographique parcourue, rester dans un petit périmètre *)
let preset_court_chemin =
  {
    name = "Court chemin";
    description = "Minimise la distance géographique parcourue.";
    criteria =
      [
        Combinator.criterion
          (module DistanceWeight)
          ~coefficient:0.75 ~label:"Distance";
        Combinator.criterion
          (module HopWeight)
          ~coefficient:0.15 ~label:"Arrêts";
        Combinator.criterion
          (module TimeWeight)
          ~coefficient:0.05 ~label:"Temps";
        Combinator.criterion
          (module CorrespondenceWeight)
          ~coefficient:0.05 ~label:"Correspondances";
      ];
  }

(* Preset 4 : Équilibré 
    Compromis entre tous les critères, sans privilégier un aspect particulier *)
let preset_equilibre =
  {
    name = "Equilibre";
    description = "Compromis entre temps, distance, arrêts et correspondances.";
    criteria =
      [
        Combinator.criterion
          (module TimeWeight)
          ~coefficient:0.35 ~label:"Temps";
        Combinator.criterion
          (module CorrespondenceWeight)
          ~coefficient:0.30 ~label:"Correspondances";
        Combinator.criterion
          (module HopWeight)
          ~coefficient:0.20 ~label:"Arrêts";
        Combinator.criterion
          (module DistanceWeight)
          ~coefficient:0.15 ~label:"Distance";
      ];
  }

(* Preset 5 : Explorateur 
    Priorise les itinéraires directs avec peu d'arrêts*)
let preset_explorateur =
  {
    name = "Explorateur";
    description = "Favorise les itinéraires directs avec peu d'arrêts.";
    criteria =
      [
        Combinator.criterion
          (module HopWeight)
          ~coefficient:0.65 ~label:"Arrêts";
        Combinator.criterion
          (module DistanceWeight)
          ~coefficient:0.10 ~label:"Distance";
        Combinator.criterion
          (module TimeWeight)
          ~coefficient:0.10 ~label:"Temps";
        Combinator.criterion
          (module CorrespondenceWeight)
          ~coefficient:0.15 ~label:"Correspondances";
      ];
  }

(* Registre complet *)
let all =
  [
    preset_rapide;
    preset_confort;
    (*preset_court_chemin;
    preset_equilibre;
    preset_explorateur;*)
  ]

(** Construit le module WEIGHT prêt à passer à Dijkstra.Make *)
let to_weight preset : (module WEIGHT with type edge = Network.connection and type node = Network.station) =
  Combinator.combine preset.criteria

(** Recherche par nom, insensible à la casse *)
let find_by_name name =
  List.find_opt
    (fun p -> String.lowercase_ascii p.name = String.lowercase_ascii name)
    all