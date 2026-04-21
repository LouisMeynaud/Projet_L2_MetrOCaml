# MetroCaml

## Statut du projet
![pipeline status](https://gitlab.com/metro-caml/metro-caml/badges/main/pipeline.svg)

## Installation
Téléchargez l'exécutable '??' publié dans la dernière [release](https://gitlab.com/metro-caml/metro-caml/-/releases) et installez les dépendances avec la commande suivante :

```bash
opam install . --deps-only
```

## Lancer le programme:

Utilisez la commande suivante (nécessite **OCaml 5.1** ou une version ultérieure) : 

```bash
dune exec metrocaml
```

### Compiler le Programme soi-même depuis le code source :
1. Cloner le dépôt git :
```bash
git clone [https://moule.informatique.univ-paris-diderot.fr/3lmtb/metrocaml.git](https://moule.informatique.univ-paris-diderot.fr/3lmtb/metrocaml.git)
```
2. Se placer dans le répertoire du projet :
```bash
cd metrocaml
```
3. Compiler avec Dune :
```bash
dune build
```
Le binaire exécutable se trouve dans le répertoire `_build` sous le nom `metrocaml`.

---

## Description:

### Présentation du projet et objectifs
Notre projet s'articule autour de deux composantes principales :

* **Une bibliothèque de graphes générique :** Conçue pour implémenter des structures de graphes et des algorithmes de parcours de manière hautement efficace. Elle a été pensée pour être la plus modulaire et abstraite possible ce qui permet de la réutiliser facilement dans des contextes totalement différents de notre application (par exemple : analyse de réseaux sociaux, modélisation de réseaux de neurones, etc.).
* **L'application MetroCaml :** Une implémentation concrète de cette bibliothèque qui propose une interface complète de recherche d'itinéraires basée sur les réseaux de transports en commun.

## MetroCaml : Un moteur de recherche d'itinéraires pour les transports en commun
### Modes de fonctionnement : En ligne / Local
Au lancement du programme, l'utilisateur peut choisir entre deux modes :

* **Mode Local :** Utilisation rapide sans connexion internet, basée sur des données pré-enregistrées (pouvant ne pas être à jour).
* **Mode En ligne :** Télécharge les données du réseau à la date du lancement. Nécessite une connexion internet. 
    * *Note :* Pour le réseau **Île-de-France**, nous utilisons une API temps réel. Les perturbations sont affichées et les temps de trajet sont adaptés dynamiquement.

**Réseaux disponibles :**

| Type | Réseaux supportés |
| :--- | :--- |
| **Local** | Toulouse, Montpellier |
| **En ligne** | Île-de-France, Toulouse, Montpellier, Marseille, SNCF, Corsica |

### Algorithmes et critères de recherche:
Notre librairie propose plusieurs algorithmes de routage, tels que **Dijkstra (amélioré)**, **BFS**, AUTRE ?.

Le projet implémente différents critères de recherche (poids) :
* **Temps :** Privilégie l'arrivée la plus tôt.
* **Distance :** Privilégie le trajet géographique le plus court.
* **Arrêts :** Minimise le nombre de stations traversées.
* **Correspondances :** Minimise les changements de lignes.

#### Profils prédéfinis
* **Rapide :** Priviligie l'heure d'arrivée tout en gardant un nombre limité de correspondances.
* **Confort :** Priorité absolue aux trajets avec peu de changements, tout en restant raisonnable sur le temps de trajet.

Lors de la recherche, vous pouvez choisir le mode **Combinaison** (pour mixer les critères manuellement) ou le mode **Défaut** (pour utiliser les profils prédéfinis).

Vous pouvez également désactiver certains modes de transport (ex: bus, métro) ou filtrer les itinéraires pour n'utiliser que des stations accessibles aux **PMR** (Personnes à Mobilité Réduite).

### Chercher un itinéraire depuis l'ui graphique:
1.  **Réglages :** Sélectionnez un algorithme, un type de poids (Défaut ou Combinaison) et activez les transports souhaités (*attention : les bus sont désactivés par défaut*).
2.  **Itinéraire :** Saisissez les stations de départ et d'arrivée, ou cliquez sur "Sélectionner" pour les choisir directement sur la carte.
3.  **Options :** Choisissez l'heure de départ et l'option d'accessibilité PMR si nécessaire.
4.  **Résultats :** Cliquez sur "Chercher". Le programme affichera les itinéraires disponibles. Saisissez le nom de l'itinéraire souhaité pour en afficher les détails.

## Benchmarks
Nous intégrons un outil de benchmark permettant de comparer les performances des algorithmes et la pertinence des critères de recherche. Il permet d'évaluer en un coup d'œil les **temps de calcul** et la **qualité des itinéraires** proposés.

## Pourquoi l'itinéraire trouvé est-il différent du vôtre ?
Plusieurs facteurs peuvent expliquer une différence de résultat :
* Les données utilisées pour le calcul ne sont pas à jour.
* Les critères de recherche choisis ne correspondent pas à vos préférences personnelles.
* Certaines lignes sont évitées par le moteur pour prévenir la surfréquentation (ex: ligne B du réseau Île-de-France).
* Les perturbations en temps réel ne sont pas prises en compte (hors IDF en ligne).
* La vitesse de marche est une moyenne et peut différer de la votre ou celle choisie par les autres moteurs de calcule d'itinéraires.
* D'après nos tests, les itinéraires d'autres moteurs de recherche ne sont pas systématiquement les "meilleurs".
* [??]

## Comment contribuer ?
Pour contribuer au projet, merci de consulter le fichier [CONTRIBUTING.md](/CONTRIBUTING.md).

## Licence
Pour visualiser la Licence, cliquer [ici](/LICENSE).

## Crédits
- LORIOT Baptiste
- MONA Matthys
- BRUNO Thomas
- DENQUIN Leo
- MOSCARDI Lorenzo
- MEYNAUD Louis