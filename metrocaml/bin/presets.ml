open Metrocaml
open Weight
open Routing

type preset = {
  name : string;
  description : string;
  astar_coef : float;
  criteria : (Network.connection, Network.station) Combinator.criterion list;
}

(* Preset 1 : Rapide
  Priorise le temps de trajet *)
let preset_rapide =
  {
    name = "Rapide";
    description = "Privilégie le temps de trajet.";
    astar_coef = 0.75;
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
    astar_coef = 0.5;
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

(* Registre complet *)
let all =
  [
    preset_rapide;
    preset_confort;
  ]

(** Construit le module WEIGHT prêt à passer aux foncteurs Make *)
let to_weight preset : (module WEIGHT with type edge = Network.connection and type node = Network.station) =
  Combinator.combine preset.criteria

(** Recherche par nom, insensible à la casse *)
let find_by_name name =
  List.find_opt
    (fun p -> String.lowercase_ascii p.name = String.lowercase_ascii name)
    all
