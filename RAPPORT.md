# Rapport - Projet OCaml

- LORIOT Baptiste (`baptiste.loriot@etu.u-paris.fr`)
- MONA Matthys (`matthys.mona@etu.u-paris.fr`)
- BRUNO Thomas (`thomas.bruno@etu.u-paris.fr`)
- DENQUIN Léo (`leo.denquin-mathelot@etu.u-paris.fr`)
- MOSCARDI Lorenzo (`lorenzo.moscardi@etu.u-paris.fr`)
- MEYNAUD Louis (`louis.meynaud@etu.u-paris.fr`)


## 1. Architecture Générale
- Organisation du projet : structure modulaire
- Trois niveaux d'abstraction :
  - **Librairie `routing/`** : noyau générique réutilisable
  - **Application `metrocaml/`** : implémentation métier
  - **Projet satellite `tiktokcaml/`** : preuve de généricité
- Dépendances externes et infrastructure (Dune, OPAM)

## 2. Librairie de Routage : Fondations Génériques
### 2.1 Abstractions et Interfaces
La bibliothèque `routing/` est conçue pour être **totalement générique**, et utilisable dans n'importe quel contexte de graphe. Elle utilise des abstractions de haut niveau pour représenter les éléments fondamentaux d'un graphe, indépendamment de leur signification. Ainsi, elle manipule une architecture modulaire basée sur des interfaces telles que `GRAPH`, `NODE` et `EDGE`, plutôt que des types précis.

Ces modules sont définis par le projet utilisant la bibliothèque. Par exemple, *MétroCaml* définit les NODEs comme des stations, les EDGEs comme des liaisons de transport... tandis que *TiktoCaml* les définit respectivement comme des utilisateurs et des relations. La bibliothèque ne connaît pas ces détails, elle se contente de manipuler des éléments génériques.

### 2.2 Algorithmes
Routing fournit plusieurs algorithmes de parcours du graphe :
  - **BFS** (parcours basique en largeur)
  - **Dijkstra** (coût minimal)
  - **A\*** (dijkstra avec biais heuristique)

### 2.3 Système de Pondération
Les algorithmes Dijkstra et A* nécessitent un système de pondération pour évaluer les coûts des chemins. La bibliothèque définit une interface `WEIGHT` qui permet de représenter une **métrique de coût**. Les projets utilisant la bibliothèque peuvent implémenter cette interface pour définir leurs propres critères de recherche. Ces modules peuvent alors utiliser les types spécifiques du projet. Par exemple, *MétroCaml* implémente des poids basés sur le temps de trajet, la distance, le nombre d'arrêts, ou les correspondances. *TiktoCaml* pourrait implémenter des poids basés sur la force de la relation, la fréquence d'interaction, etc.

Routing fournit aussi un système de **combinaison de poids**, `Combinator`, permettant de créer des métriques composites à partir de plusieurs critères. Par exemple, un utilisateur pourrait vouloir minimiser à la fois le temps de trajet et le nombre de correspondances, en pondérant ces deux critères selon ses préférences.

### 2.4 Résultats et Itinéraires
Les algorithmes de recherche retournent des résultats sous forme de `PATH`, une interface définie par la bibliothèque et paramétrée par les types `NODE` et `EDGE` du projet, qui contient la liste des arêtes parcourues.

### 2.5 Benchmarking et Performance
La bibliothèque inclut des outils de **benchmarking** pour évaluer la performance des différents algorithmes, soit sur un itinéraire spécifique, soit sur un nombre de cas de tests aléatoires. Ces outils permettent de mesurer le temps de calcul et la qualité des itinéraires proposés (en fonction des critères de pondération).

### 2.6 Démonstration de la généricité
Ici, la bibliothèque routing est utilisée dans deux contextes totalement différents : les **transports** (*MétroCaml*, le projet principal) et les **réseaux sociaux** (*TiktoCaml*, un mini-projet annexe). Cela prouve que la conception générique de la bibliothèque permet de l'adapter facilement à différents domaines, sans modifications structurelles majeures.

#### A | MetroCaml : Transports
- Noeuds = stations/arrêts
- Arêtes = trajets (bus, métro, marche, transfert...)
- Poids  = critères (temps, distance, correspondances, nombre d'arrêts)

#### B | *TiktoCaml* : Réseaux Sociaux
- Noeuds = personnes
- Arêtes = relations d'amitié
- Poids  = critères de parcours


## 3. Application *MétroCaml*
*MétroCaml* est un moteur de recherche d'itinéraires pour les transports. Il propose une interface complète pour trouver des trajets entre deux points d'un réseau en fonction de différents critères de recherche.

### 3.1 Modèle de Données
Pour charger des données réelles, *MétroCaml* s'appuie sur le standard **GTFS (General Transit Feed Specification)**. Il s'agit d'un format universel de représentation des réseaux de transport, structuré sous la forme d'archives contenant des fichiers textes CSV. Nous avons choisi ce format car c'est le standard officiel en Europe et à l'international. Cette standardisation nous offre une grande flexibilité et permet à notre moteur de calculer des itinéraires sur des réseaux très variés, allant de Tisséo (Toulouse) à Île-de-France Mobilités. Concrètement, une fois les données téléchargées, notre parseur lit ces fichiers **CSV** et les modélise directement en mémoire sous la forme d'un objet **TransportGraph**.
Le graphe statique est complété par un modele en temps réel. Ce module s'interface avec l'API PRIM d'Île-de-France Mobilités pour récupérer les flux d'informations trafic au format **JSON (GTFS-RT)**. Cette intégration s'effectue via un **thread parallèle** afin de ne pas bloquer les modules de calcul d'itinéraires.

### 3.2 Interface Utilisateur
Deux interfaces sont disponibles au lancement de *MétroCaml* :
- une **interface console** (CLI) pour un affichage textuel
- une **interface graphique** (GUI) pour un affichage visuel interactif et l'intégration des alertes et perturbations en temps réel.

Les données peuvent être chargées en local ou en ligne.

### 3.3 Critères de Recherche et Profils
L'utilisateur a la possibilité de personnaliser sa recherche avec plusieurs critères sous forme de poids. Voici une liste exhaustive des différents critères :
  - **Temps** : minimise l'heure d'arrivée
  - **Distance** : privilégie la trajectoire géographique
  - **Arrêts** : minimise le nombre de stations parcourues
  - **Correspondances** : réduit le nombre de changements de lignes
L'utilisateur peut également utiliser des profils combinés prédéfinis comme **Rapide** et **Confort**, ou bien définir manuellement la pondération de chaque critère pour une recherche personnalisée.

### 3.4 Modes de Fonctionnement
- **Mode Local** : données pré-chargées, pas de connexion requise
- **Mode En Ligne** : données temps réel, APIs externes
- Réseaux disponibles : Île-de-France, Toulouse, Montpellier, Marseille, SNCF, Corsica
- Perturbations temps réel (notamment IDF) et accessibilité PMR

## 4. Processus de Développement

#### Répartition du travail au sein de l'équipe
- **Léo** : 18%
- **Baptiste** : 17%
- **Louis** : 17%
- **Thomas** : 17%
- **Matthys** : 16%
- **Lorenzo** : 15%

### 4.1 Gestion de Projet et Git
Le projet a suivi le workflow Git Flow :

- Branches `feature/[feature]` ou `[package]/[feature]` pour chaque développement, `fix/[fix]` pour les corrections de bug.
- Merge Requests (MR) dans `dev` obligatoires avec revue de code par au moins trois autres membres.
- Branche `dev` pour les sprints.
- Branche `main` uniquement pour les releases stables (MR unique dev -> main).

Méthodologie agile avec des sprints et des releases, et un suivi des tâches via les tickets GitLab.

### 4.2 Intégration Continue (CI/CD)
Une pipeline GitLab CI a été mise en place pour le projet (`.gitlab-ci.yml`) :

- `setup` : Installation des dépendances via OPAM.

- `lint` : Analyse statique du code avec OCamlFormat pour garantir un code conforme.

- `test` : Exécution des tests avec Alcotest. Utilisation de Bisect_ppx pour mesurer la couverture de code, avec un seuil minimal imposé (à 70%). La pipeline délivre un artéfact contenant un fichier html avec le rapport de couverture, ainsi que le rapport XML pour l’intégration dans GitLab.

- `release` : Création automatique d'une release GitLab quand un tag est push.

- `docs` : Génération de la documentation avec OCamlDoc.

Nous voulions initialement publier automatiquement le rapport de couverture et la doc sur GitLab Pages, mais cette fonctionnalité est désactivée sur le serveur Moule.

### 4.3 Tests et Qualité
- Suite de **tests unitaires** (avec *alcotest*)
- Benchmarking : comparaison des performances d'algorithmes / **tests de performance**
- Une fois un bug corrigé, un **test de régression** est ajouté.
- Couverture de code (avec *bisect_ppx*) : seuil minimal de 70% pour valider les MR.
- **Tests de compatibilité** :
    - exécution du projet sur différents systèmes (Linux, Windows, MacOS)
    - exécution de *MétroCaml* sur différents réseaux (IDF, Toulouse, etc.) pour vérifier la robustesse du parseur GTFS.


# STATISTIQUES - BENCHMARKS

# Ile-de-France

## Benchmark unique

### MetroCaml — de *Moulin de Grais* à *Pépinière*

Algorithme / Preset         | Temps         | Résultat                                                            
--------------------------- | ------------- | ------------------------------------------------------------------- 
Dijkstra — Preset *Rapide*  | 26 786.370 ms | OK — 41 étapes · 2 h 15 min · Arrivée à 00:13 · 3 correspondances   
A* — Preset *Rapide*        | 22 443.797 ms | OK — 41 étapes · 2 h 15 min · Arrivée à 00:13 · 3 correspondances   
Dijkstra — Preset *Confort* | 31 222.990 ms | OK — 41 étapes · 2 h 15 min · Arrivée à 00:13 · 3 correspondances   
A* — Preset *Confort*       | 25 363.036 ms | OK — 41 étapes · 2 h 15 min · Arrivée à 00:13 · 3 correspondances   
Dijkstra — Temps            | 16 652.714 ms | OK — 41 étapes · 2 h 15 min · Arrivée à 00:13 · 3 correspondances   
Dijkstra — Distance         | 62 792.586 ms | OK — 64 étapes · 45 h 13 min · Arrivée à 19:11 · 32 correspondances 
Dijkstra — Arrêts           | 3 452.742 ms  | OK — 12 étapes · 31 h 21 min · Arrivée à 05:19 · 7 correspondances  
Dijkstra — Correspondances  | 38 376.573 ms | OK — 345 étapes · 35 h 44 min · Arrivée à 09:42 · 2 correspondances 
A* — Temps                  | 16 277.596 ms | OK — 41 étapes · 2 h 15 min · Arrivée à 00:13 · 3 correspondances   
A* — Distance               | 53 288.690 ms | OK — 71 étapes · 3 h 13 min · Arrivée à 01:11 · 9 correspondances   
A* — Arrêts                 | 23 883.126 ms | OK — 37 étapes · 2 h 49 min · Arrivée à 00:47 · 9 correspondances   
A* — Correspondances        | 15 763.271 ms | OK — 79 étapes · 2 h 41 min · Arrivée à 00:39 · 4 correspondances   
Arrêts (BFS)                | 2 936.131 ms  | OK — 12 étapes · 34 h 17 min · Arrivée à 08:15 · 7 correspondances  


## Benchmark aléatoire

> * **Catégorie :** Distance haversine entre le point de départ et le point d'arrivé
> * **Dijkstra TW :** Temps d'exécution de Dijkstra supérieur à A* 
> * **Dijkstra WW :** Poids de Dijkstra inférieur à celui trouvé par A*
> * **Dijkstra/A\* (ms) :** Moyenne du temps d'exécution de l'algorithme
> * **Dijkstra/A\* w :** Moyenne du poids trouvé par l'algorithme 


Catégorie      | Bench | Dijkstra TW | Dijkstra WW| Dijkstra (ms) | A* (ms) | Dijkstra w |   A* w |
-------------- | ----: | ----------: | ---------: | ------------: | ------: | ---------: | -----: |
< 8000         |     2 |      50.0 % |     75.0 % |        0.6511 |  0.6809 |     0.5000 | 1.2500 |
[8000, 15000]  |     5 |      20.0 % |     70.0 % |        3.3885 |  2.3598 |     1.7000 | 1.9000 |
[15000, 25000] |    10 |      30.0 % |     85.0 % |        8.0189 |  8.2559 |     2.3000 | 2.4000 |
[25000, 35000] |    10 |      40.0 % |     65.0 % |       19.0318 | 16.8218 |     2.4500 | 2.9000 |
≥ 35000        |    23 |      26.1 % |     76.1 % |       41.2837 | 37.1742 |     2.9783 | 3.1522 |

### Résumé global

* **Total des benchmarks :** 50
* **A\* gagnant sur le temps d'exécution :** 35 cas
* **Dijkstra gagnant sur le poids (temps / correspondances) :** 37 cas

> **Statistique globale :**
> A* est meilleur dans **70.00 %** des cas que Djikstra en temps d’exécution,
> et dans **75.00 %** des cas Djikstra a de meilleurs résultats en termes de poids (*Time / Transfers*).

---

# Toulouse

## Résultats du benchmark aléatoire

Catégorie      | Bench | Dijkstra TW | Dijkstra WW| Dijkstra (ms) | A* (ms) | Dijkstra w |   A* w |
-------------- | ----: | ----------: | ---------: | ------------: | ------: | ---------: | -----: |
< 3000         |   111 |      38.7 % |     88.3 % |        0.0406 |  0.0343 |     0.1892 | 0.3333 |
[3000, 8000]   |   463 |      37.1 % |     85.5 % |        0.1231 |  0.1082 |     1.0961 | 1.3164 |
[8000, 13000]  |   581 |      36.5 % |     86.4 % |        0.2338 |  0.2054 |     1.7797 | 1.9707 |
[13000, 20000] |   592 |      41.4 % |     88.7 % |        0.2919 |  0.2696 |     2.3454 | 2.4865 |
≥ 20000        |   253 |      40.7 % |     91.3 % |        0.3539 |  0.3237 |     3.1364 | 3.2411 |

### Résumé global

* **Total des benchmarks :** 2000
* **A\* gagnant sur le temps d'exécution :** 1225 cas
* **Dijkstra gagnant sur le poids (temps / correspondances) :** 3505 cas

> **Statistique globale :**
> A* est meilleur dans **62.25 %** des cas que Djikstra en temps d’exécution,
> et dans **87.62%** des cas Djikstra a de meilleurs résultats en termes de poids (*Time / Transfers*).
---

## Conclusion Générale
Dijkstra a souvent de meilleurs résultats que A*, on le voit bien avec les deux derniers benchs (75.00% et 87.62% où Dijkstra a des résultats meilleurs ou semblables à A*). Néanmoins A* est quant à lui plus rapide en temps d'exécution que Dijkstra (70.00% et 62.25% où A* est plus rapide que Djikstra) surtout sur les longues distances où A* est meilleur à 80% en temps d'exécution par rapport à Dijkstra.
A* est plus rapide que Djikstra dans la majorité des cas mais souvent Djikstra a un meilleur itinéraire selon les critères sélectionnés.