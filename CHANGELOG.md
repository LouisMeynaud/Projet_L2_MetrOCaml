Changelog v0.1.3

### Interface graphique (GUI) :
- optimisation de l'affichage graphique
- multiples corrections esthétiques
- ajout de l'affichage des résultats trouvés directement dans l'interface
- ajout des alertes en temps réel pour IDFM et de leur affichage

### UI console (CLI) :
- ajout de la prise en charge des horaires
- ajout de la prise en charge des modes de transports à exclure
- ajout de la prise en charge du critère PMR

### Parseur & Données :
- ajout de nouveaux réseaux de transport
- ajout de la prise en charge de l'accessibilité PMR (Personnes à Mobilité Réduite) dans le parseur

### Algorithmes & Poids :
- ajout de la possibilité de filtrer et de ne pas prendre certains modes de transport
- ajout de la possibilité de limiter les correspondances uniquement aux stations accessibles PMR
- correction du routage : l'algorithme préfère désormais attendre à une station plutôt que de marcher si c'est possible
- implémentation de la prise en compte dans la recherche d’itinéraire des perturbations et blocages
- amélioration de l’efficacité de la recherche

### Presets :
- équilibrage des profils de recherche "Rapide" et "Confort"
- suppression des profils “Equilibre”, “Court Chemin” et “Explorateur”

### Général :
- ajout du choix du mode de chargement des données (en ligne ou local)
- ajout de la possibilité de définir une adresse de départ plutôt qu'une station
- correction : possibilité de lancer plusieurs recherches à la suite sans devoir relancer l'application
- refonte et nettoyage du code du `main`
- ajout de benchmarks pour évaluer les performances des algorithmes et des poids
- ajout d'un logger coloré pour le suivi et le débogage
- ajout de TiktoCaml (projet annexe)
