# Rapport - Projet OCaml

- LORIOT Baptiste (`baptiste.loriot@etu.u-paris.fr`)
- MONA Matthys (`matthys.mona@etu.u-paris.fr`)
- BRUNO Thomas (`thomas.bruno@etu.u-paris.fr`)
- DENQUIN Léo (`leo.denquin-mathelot@etu.u-paris.fr`)
- MOSCARDI Lorenzo (`lorenzo.moscardi@etu.u-paris.fr`)
- MEYNAUD Louis (`louis.meynaud@etu.u-paris.fr`)


## 1. Introduction
bla bla bla

## 2. Architecture Générale
- Organisation du projet : structure modulaire
- Trois niveaux d'abstraction :
  - **Librairie `routing/`** : noyau générique réutilisable
  - **Application `metrocaml/`** : implémentation métier
  - **Projet satellite `tiktokcaml/`** : preuve de généricité
- Dépendances externes et infrastructure (Dune, OPAM)

## 3. Librairie de Routage : Fondations Génériques
### 3.1 Abstractions et Interfaces
La bibliothèque `routing/` est conçue pour être **totalement générique**, et utilisable dans n'importe quel contexte de graphe. Elle utilise des abstractions de haut niveau pour représenter les éléments fondamentaux d'un graphe, indépendamment de leur signification. Ainsi, elle manipule une architecture modulaire basée sur des interfaces telles que `GRAPH`, `NODE` et `EDGE`, plutôt que des types précis.

Ces modules sont définis par le projet utilisant la bibliothèque. Par exemple, *MétroCaml* définit les NODEs comme des stations, les EDGEs comme des liaisons de transport... tandis que *TiktoCaml* les définit respectivement comme des utilisateurs et des relations. La bibliothèque ne connaît pas ces détails, elle se contente de manipuler des éléments génériques.

### 3.2 Algorithmes
Routing fournit plusieurs algorithmes de parcours du graphe :
  - **BFS** (parcours basique en largeur)
  - **Dijkstra** (coût minimal)
  - **A\*** (dijkstra avec biais heuristique)

### 3.3 Système de Pondération
Les algorithmes Dijkstra et A* nécessitent un système de pondération pour évaluer les coûts des chemins. La bibliothèque définit une interface `WEIGHT` qui permet de représenter une **métrique de coût**. Les projets utilisant la bibliothèque peuvent implémenter cette interface pour définir leurs propres critères de recherche. Ces modules peuvent alors utiliser les types spécifiques du projet. Par exemple, MétroCaml implémente des poids basés sur le temps de trajet, la distance, le nombre d'arrêts, ou les correspondances. TiktoCaml pourrait implémenter des poids basés sur la force de la relation, la fréquence d'interaction, etc.

Routing fournit aussi un système de **combinaison de poids**, `Combinator`, permettant de créer des métriques composites à partir de plusieurs critères. Par exemple, un utilisateur pourrait vouloir minimiser à la fois le temps de trajet et le nombre de correspondances, en pondérant ces deux critères selon ses préférences.

### 3.4 Résultats et Itinéraires
Les algorithmes de recherche retournent des résultats sous forme de `PATH`, une interface définie par la bibliothèque et paramétrée par les types `NODE` et `EDGE` du projet, qui contient la liste des arêtes parcourues.

### 3.5 Benchmarking et Performance
La bibliothèque inclut des outils de **benchmarking** pour évaluer la performance des différents algorithmes, soit sur un itinéraire spécifique, soit sur un nombre de cas de tests aléatoires. Ces outils permettent de mesurer le temps de calcul et la qualité des itinéraires proposés (en fonction des critères de pondération).

### 3.6 Démonstration de la généricité
Ici, la bibliothèque routing est utilisée dans deux contextes totalement différents : les **transports** (*MétroCaml*, le projet principal) et les **réseaux sociaux** (*TiktoCaml*, un mini-projet annexe). Cela prouve que la conception générique de la bibliothèque permet de l'adapter facilement à différents domaines, sans modifications structurelles majeures.

#### A | MetroCaml : Transports
- Noeuds = stations/arrêts
- Arêtes = trajets (bus, métro, marche, transfert...)
- Poids  = critères (temps, distance, correspondances, nombre d'arrêts)

#### B | TiktoCaml : Réseaux Sociaux
- Noeuds = personnes
- Arêtes = relations d'amitié
- Poids  = critères de parcours


## 5. Application MétroCaml
MétroCaml est un moteur de recherche d'itinéraires pour les transports. Il propose une interface complète pour trouver des trajets entre deux points d'un réseau en fonction de différents critères de recherche.

### 5.1 Modèle de Données
- Parseur GTFS (General Transit Feed Specification)
  - Format standardisé (CSV)
  - Intégration depuis sources publiques (IDF, Toulouse, Montpellier, etc.)
  - Adaptateur pour la structure graphe générique
// THOMAS complète ça

### 5.2 Interface Utilisateur
Deux interfaces sont disponibles au lancement de MétroCaml :
- une **interface console** (CLI) pour un affichage textuel
- une **interface graphique** (GUI) pour un affichage visuel interactif et l'intégration des alertes et perturbations en temps réel.

Les données peuvent être chargées en local ou en ligne.

### 5.3 Critères de Recherche et Profils
L'utilisateur a la possibilité de personnaliser sa recherche avec plusieurs critères sous forme de poids. Voici une liste exhaustive des différents critères :
  - **Temps** : minimise l'heure d'arrivée
  - **Distance** : privilégie la trajectoire géographique
  - **Arrêts** : minimise le nombre de stations parcourues
  - **Correspondances** : réduit le nombre de changements de lignes
L'utilisateur peut également utiliser des profils combinés prédéfinis comme **Rapide** et **Confort**, ou bien définir manuellement la pondération de chaque critère pour une recherche personnalisée.

### 5.4 Modes de Fonctionnement
- **Mode Local** : données pré-chargées, pas de connexion requise
- **Mode En Ligne** : données temps réel, APIs externes
- Réseaux disponibles : Île-de-France, Toulouse, Montpellier, Marseille, SNCF, Corsica
- Perturbations temps réel (notamment IDF) et accessibilité PMR

## 6. Processus de Développement

#### Répartition du travail au sein de l'équipe
- **Baptiste** : 
- **Matthys** :
- **Thomas** :
- **Léo** :
- **Lorenzo** :
- **Louis** :

### 6.1 Gestion de Projet et Git
Le projet a suivi le workflow Git Flow :

- Branches `feature/[feature]` ou `[package]/[feature]` pour chaque développement, `fix/[fix]` pour les corrections de bug.
- Merge Requests (MR) dans `dev` obligatoires avec revue de code par au moins trois autres membres.
- Branche `dev` pour les sprints.
- Branche `main` uniquement pour les releases stables (MR unique dev -> main).

Méthodologie agile avec des sprints et des releases, et un suivi des tâches via les tickets GitLab.

### 6.2 Intégration Continue (CI/CD)
Une pipeline GitLab CI a été mise en place pour le projet (`.gitlab-ci.yml`) :

- `setup` : Installation des dépendances via OPAM.

- `lint` : Analyse statique du code avec OCamlFormat pour garantir un code conforme.

- `test` : Exécution des tests avec Alcotest. Utilisation de Bisect_ppx pour mesurer la couverture de code, avec un seuil minimal imposé (à 70%). La pipeline délivre un artéfact contenant un fichier html avec le rapport de couverture, ainsi que le rapport XML pour l’intégration dans GitLab.

- `release` : Création automatique d'une release GitLab quand un tag est push.

- `docs` : Génération de la documentation avec OCamlDoc.

Nous voulions initialement publier automatiquement le rapport de couverture et la doc sur GitLab Pages, mais cette fonctionnalité est désactivée sur le serveur Moule.

### 6.3 Tests et Qualité
- Suite de **tests unitaires** (avec *alcotest*)
- Benchmarking : comparaison des performances d'algorithmes / **tests de performance**
- Une fois un bug corrigé, un **test de régression** est ajouté.
- Couverture de code (avec *bisect_ppx*) : seuil minimal de 70% pour valider les MR.
- **Tests de compatibilité** :
    - exécution du projet sur différents systèmes (Linux, Windows, MacOS)
    - exécution de MétroCaml sur différents réseaux (IDF, Toulouse, etc.) pour vérifier la robustesse du parseur GTFS.


# STATISTIQUES - BENCHMARKS
[[INSÉRER MATTHYS ICI]]