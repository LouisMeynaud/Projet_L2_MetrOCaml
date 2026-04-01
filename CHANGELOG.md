Changelog v1.2
### Nouvelle interface graphique :
- nouveaux visuels
- utilisation de vrais boutons
- utilisation des poids personnalisés
- suppression des caches de dessins (optimisation)
- mise en cache des textes (optimisation)
- ajout de l'onglet horaires (permet de voir les horaires de passage pour un arrêt/ligne)
- nombreuses autres optimisations

### Ajout UI console :
- utilisation des poids personnalisés
- affichage des chemins trouvés (détaillé ou simple)
- ajout du temps et de la distance dans l'affichage pour un chemin choisi
- possibilité de choisir entre plusieurs réseaux au lancement (Toulouse et IDF par exemple)
- ajout de tolérance lors des questions à choix multiples / demandes de stations (la casse et les accents ne comptent plus)

### Parseur :
- ajout des distances entre les stations
- résolution de bugs sur les horaires
- optimisation du chargement des fichiers à parser
- ajout des connexions de transfert entre les stations proches

### Poids :
- refonte des modules weight pour qu'ils prennent en compte les horaires des trains
- ajout dans l'affichage des temps d'attente entre les correspondances

### Presets :
- ajout des presets (combinaisons de poids prédéfinis) (Rapide, Confort, Équilibré, Explorateur, Court chemin)

### Général :
- séparation du code de main et de la console
- ajout d'une licence
- vérification de si des datas sont déjà téléchargées, dans ce cas chargement des données locales
- possibilité d'entrer des coordonnées GPS plutôt qu'une station (estimation du temps de marche)