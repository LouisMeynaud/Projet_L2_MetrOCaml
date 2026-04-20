# MetroCaml

## Statues du projet
![pipeline status](https://gitlab.com/metro-caml/metro-caml/badges/main/pipeline.svg)


## Installation
Télécharger l'exécutable '??' publié dans la dernière release [release](https://gitlab.com/metro-caml/metro-caml/-/releases) et installer les dépendances avec la commande suivante :
```bash
opam install . --deps-only
```

## Lancer le programme :

utiliser la commande suivante avec version ocaml 5.1 ou une version ultérieure : 

```bash
dune exec metrocaml
```

#### Compiler le Programme soi-même depuis le code source :
Cloner le dépôt git :
```bash
git clone https://moule.informatique.univ-paris-diderot.fr/3lmtb/worms-mi4.git
```
Se placer dans le répertoire du projet :
```bash
cd metrocaml
```
Compiler le projet avec Dune :
```bash
dune build
```
Le binaire exécutable se trouve dans le répertoire `._build` sous le nom `metrocaml`.

---

## Description : 

###Presentation du projet et objectifs

### En ligne/Local:
Au lancement du programme, l'utilisateur aura la possibilité de choisir entre le mode en ligne et le mode local.

-le mode local offre la possibilité d'utiliser le programme de maniere rapide et sans connexion internet, mais avec des données non a jour.

-le mode en ligne telechargeras les données du reseaux choisis a la date du lancement du programme, mais necessite une connexion internet et peut prendre plus de temps a se lancer. 
Pour le reseaux Ile-de-France nous utilisont une API qui collecte les données en temps réel, les perturbation seront alors affichée et les temps de trajet adapter.

Les reseaux disponibles en local sont : 
- Toulouse
- Monpellier

Les reseaux disponibles en ligne sont :
- Ile-de-France
- Toulouse
- Monpellier 
- Marseille
- SNCF
- Corsica


### algorithmes et critère de recherche:
Notre librairie propose plusieurs algorithmes de routage, tels que Dijkstra (amélioré),Bfs, ...,

Le projet quand a lui implemente differents critères de recherche (les poids) tels que :
-temps : priviligie l'arrivé la plus tot
-distance : priviligie le trajet le plus court
-arret : priviligie le trajet avec le moins d'arret
-correspondance : priviligie le trajet avec le moins de correspondance

et Profile, des combinaisons prefaite de ces critères, par exemple :
-rapide : accorde une grande importance a l'heure d'arrivé mais aussi une legère au nombre de correspondance
-confort : accorde une grande importance au trajet avec peu de correspondance et une legère au temps d'arrivé

lors de la recherche d'itinéraire, l'utilisateur poura choisir entre le critère combinaison , qui permet de combiner les différents critère de recherche , ou default qui lui permettra de choisir entre les different profile predefinis.

il vous sera aussi possible de desactivé certain moyen de transport tel que le bus ou le metro par exemple, ou bien de choisir d'autoriser les correspondance uniquement dans les stations equipé de norme accessibilité.(pmr)

### chercher un itinéraire depuis l'ui graphique: 
Pour chercher un itinéraire, dans l'onglet reglage, selectionner un algortihme, un poids parmis default et combinaison, et activé/desactivé les moyens de transport que vous souhaitez(attention bus desactivé par defaut), puis dans l'onglet itineraire , au choix taper une station de départ ou d'arrivé ou cliquer sur selectionnée et selectionner la station sur la carte.choisissez ensuite une heure de depart, si vous souhaitez faire des correspondance uniquement dans les stations equipé de norme d'accessibilité au personne a mobilité reduite, et enfin cliquer sur le bouton chercher.Le programme proposera different itineraire disponible, écriver le nom de l'itinéraire souhaiter pour voir les detailles de celui ci.


## les benchmarks
nous proposons un outil de benchmark qui permet de comparer les différents algorithmes et critères de recherche sur des itinéraires spécifiques. qui permet de comparer en un coup d'oeil les temps de calcul et la qualité des itinéraires proposés.


## itinaire trouver different du votre ? les raison:
- les données utilisées pour le calcul de l'itinéraire ne sont pas à jour
- les critères de recherche choisis ne correspondent pas à vos préférences
- certaines ligne sont evité par votre moteur de routage pour evité les surfrequentation (exemple ligne B du reseaux Ile-de-France)
- les perturbation en temps réel ne sont pas pris en compte dans le mode local
- la vitesse de marche est une vitesse moyenne et ne correspond pas à votre vitesse de marche réelle.
- d'après nos tests, tout au long du developpement du projet, les itinéraires proposés par d'autre moteur de recherche ne sont pas forcement les meilleurs.
- autre raison ???



## Comment contribuer ?

Pour contribuer à ce projet, se référer au fichier [CONTRIBUTING.md](/CONTRIBUTING.md).

## Licence

Pour visualiser la Licence, cliquer [ici](/LICENSE).

## Crédits

- LORIOT Baptiste
- MONA Matthys
- BRUNO Thomas
- DENQUIN Leo
- MOSCARDI Lorenzo
- MEYNAUD Louis

