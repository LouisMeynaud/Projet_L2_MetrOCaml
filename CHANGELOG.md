Changelog v1.0

### Nouveautés et Fonctionnalités
- **Algorithme :** Intégration de l'algorithme de recherche A* (A-star).

- **Perturbations :** Le moteur de calcul intègre désormais un temps d'attente additionnel lorsqu'une perturbation est détectée sur le réseau.
- **Performances :** Mise en place d'un système de benchmark basé sur des entrées aléatoires pour évaluer l'efficacité des algorithmes.
Modification du système de benchmarks pour le rendre générique et évaluer les différents applications  
- Utilisation de dotenv
- Ajout d’un job CD pour publier les exécutables dans la release GitLab.
- **Maintenance & Qualité :** Ajout de tests, rédaction de la documentation du code et ajout du fichier `README.md`, `DESIGN.md`.

### Corrections de bugs
- **Affichage :** Résolution d'un problème d'affichage concernant les temps de trajet dans la
 sortie console (`printpath`).
- **GUI :** Résolution d’un bug ne permettant plus la sélection d’une station.
- **GUI :** Amélioration de plusieurs détails esthétiques
- **Routing:** : Uniformisation des noms génériques
- Amélioration des algorithmes (terminaison plus rapide pour les chemins suboptimaux)
- Optimisation du code (alternatives en temps constant, mémoisation…)
- Standardisation des signatures/interfaces dans routing/ avec .mli
- Interface ALGO + générique en prenant `init` plutôt que `start_time`
- Fix sélection des modes de transports dans le GUI
- Suppression de code obsolète
- Gestion des ressources GTFS en cas d’erreur + race conditions

