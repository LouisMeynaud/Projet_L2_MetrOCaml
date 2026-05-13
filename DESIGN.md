# Décisions importantes de conception du projet

Ce document décrit les décisions de conception clés prises lors du développement de ce 
projet. Il vise à fournir un contexte et une justification pour les choix techniques et 
architecturaux effectués.



## 1. Architecture Modèle-Vue-Contrôleur (MVC)

Le projet a été conçu en respectant scrupuleusement l'architecture MVC (Modèle-Vue-Contrôleur). Ce choix garantit une séparation stricte des responsabilités, facilite la maintenance et le dévelopement du code. Cette logique s'applique à deux niveaux : sur l'architecture globale du projet et au cœur même de l'interface graphique.

### A) Architecture Globale
Au niveau de l'application, les composants sont divisés de la manière suivante :
*   **Modèle :** Gère la logique métier, les structures de données et les algorithmes. Il regroupe l'ensemble de types `Network.ml` (inhérents au réseau de transport), la structure du graphe (`Graph.ml`), la définition des poids (`Weight.ml`), ainsi que les algorithmes de routage (Dijkstra, A*, BFS).
*   **Contrôleur :** Pilote l'exécution globale du programme. Il est représenté par `Runner` et le `Main`, qui orchestrent le déroulement de l'application et lient le modèle aux différentes vues.
*   **Vue :** L'interface utilisateur est déclinée en deux implémentations distinctes : l'interface console (CLI) et l'interface graphique (GUI).

### B) Architecture Interne de l'Interface Graphique (GUI)
Afin de garder un code lisible, maintenable et d'éviter l'éparpillement des valeurs "en dur", l'interface graphique (GUI) adopte elle-même son propre découpage MVC :
*   **Modèle (`gui_types.ml`) :** Centralise toutes les constantes visuelles (tailles des éléments, espacements) et stocke l'état. Ce dernier est un "Record" qui conserve l'état dynamique de l'interface à l'instant T (station sélectionnée, onglet actif, options cochées).
*   **Vue (`gui_view.ml`** et **`gui_draw.ml`) :** Se concentre purement sur le rendu visuel. Ces modules instancient les éléments graphiques (boutons, onglets, agencement) et se chargent du dessin direct sur le canevas.
*   **Contrôleur (`gui_controller`) :** Gère la logique interactive. Il crée les connexions entre les différents éléments graphiques (clics sur les boutons, interactions avec la carte, molette) et l'état. C'est également lui qui se charge d'appeler les algorithmes du Modèle lors de la validation d'une recherche d'itinéraire.

### C) Découplage et Généricité (`transport_view`)
Le pont entre le Modèle métier et la Vue graphique est assuré par un module d'adaptation : `transport_view.ml`. Ce choix architectural offre deux avantages majeurs :
*   **Optimisation des performances :** La GUI n'a pas besoin de connaître la structure complexe et lourde du graphe entier. Le module d'adaptation filtre les données pour ne fournir à l'interface que le strict nécessaire à l'affichage (ex : coordonnées géographiques et couleurs des arêtes à dessiner).
*   **Généricité :** L'interface graphique est ignorante vis-à-vis des données qu'elle affiche. Grâce à cette abstraction, le moteur graphique est facilement réutilisable. Il suffirait de créer un nouvel adaptateur (par exemple `tiktocaml_view`) pour utiliser exactement la même interface visuelle sur le projet TiktoCaml, sans avoir à réécrire la moindre ligne de code graphique.

## 2. L’optimisation de Dijkstra / A*

### A) Le routage par état
L'algorithme mémorise des états complets plutôt que uniquement des noeuds, par exemple sur *MetroCaml*, il enregistre un état composé d’une station et d’une ligne. Cela permet de conserver en mémoire tous les chemins différents qui arrivent à une même station et donc d’éviter de supprimer des chemins qui pourraient devenir plus intéressants plus tard. Par exemple, dans le contexte de *MetroCaml*, de part les correspondances, un chemin arrivant 1 minute plus tard qu’un autre à une même station mais avec une meilleure ligne peut partir plus vite.

### B) Le hachage spécialisé
Création du module StateHash qui utilise explicitement *G.Node.hash* au lieu du hachage polymorphique d'OCaml. Le hachage par défaut d'OCaml est polymorphique (il analyse la structure complète de l'objet à l'exécution), ce qui est très lent sur des millions d'appels. En forçant l'utilisation d'une fonction de hachage spécifique au nœud, on accélère fortement les opérations de lecture/écriture dans les tables *dist* et *prev*.

### C) Le tas d’appariement
On implémente un tas d’appariement sur mesure pour stocker les chemins potentiels plutôt qu’une liste ou encore un tableau. L’insertion à une complexité en *O*(1) et la réorganisation se fait uniquement à l’extraction. Cela réduit donc la complexité de l’algorithme dans la gestion des chemins potentiels, très nombreux notamment à cause du routage par état, et permet d’en améliorer l’efficacité.

### D) Le découplage total
L’algorithme délègue l’évaluation mathématiques aux modules de poids. Cela permet une grande généricité de Dijkstra et A* que ça soit pour calculer différents types de poids pour un même graphe ou même pour différents graphes.

### E) Filtrage dynamique des transitions
Permet d’interdire certains mouvements dans un nœud sans modifier le graphe. Par exemple, pour *MetroCaml*, on peut interdire une correspondance s’il n’y a pas d’accès PMR dans la station sans modifier le graphe et en autorisant toujours de rester sur la même ligne.

### F) L’arrêt immédiat
L'algorithme s'arrête et reconstruit le chemin dès la première fois que la destination sort du tas. C'est une propriété mathématique des files de priorité : la première fois qu'un nœud est extrait du tas, c'est absolument garanti que c'est par le chemin le plus court possible. L'algorithme coupe immédiatement la recherche, économisant potentiellement l'exploration de centaines d'autres stations inutiles.